(ns pbtic.birthday.csv-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :refer [for-all]]
            [pbtic.birthday.csv :as csv]))

;;;;;;;
;; defs

(def ^:private text-data
  (str "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
       ":;<=>?@ !#$%&'()*+-./[\\]^_`{|}~"))

;;;;;;;;;;;;;
;; generators

(defn- text [cs]
  (gen/let [xs (gen/list (gen/elements cs))]
    (apply str xs)))

(def unquoted-text (text text-data))
(def quotable-text (text (str text-data "\r\n\",")))


(def field (gen/one-of [unquoted-text, quotable-text]))


(def header (partial gen/vector field))
(def record (partial gen/vector field))


(defn entry [size ks]
  (gen/let [vs  (record size)]
    (zipmap ks vs)))


(def csv-source
  (gen/let [size    gen/pos-int
            ks      (header (inc size))]
    (gen/list (entry (inc size) ks))))

;;;;;;;;;;;;;
;; properties

(defspec roundtrip-encoding-decoding
  (for-all [maps  csv-source]
    (= maps (csv/decode (csv/encode maps)))))

;;;;;;;;
;; tests

(deftest one-column-csv-files-are-inherently-ambiguous
  (is (= "\r\n\r\n\r\n"
         (csv/encode [{"" ""}, {"" ""}])))

  (is (= [{"" ""}]
         (csv/decode "\r\n\r\n"))))


(deftest one-record-per-line
  (is (= [{"aaa" "zzz", "bbb" "yyy", "ccc" "xxx"}]
         (csv/decode "aaa,bbb,ccc\r\nzzz,yyy,xxx\r\n"))))


(deftest optional-trailing-crlf
  (is (= [{"aaa" "zzz", "bbb" "yyy", "ccc" "xxx"}]
         (csv/decode "aaa,bbb,ccc\r\nzzz,yyy,xxx"))))


(deftest double-quotes
  (is (= [{"aaa" "zzz", "bbb" "yyy", "ccc" "xxx"}]
         (csv/decode "\"aaa\",\"bbb\",\"ccc\"\r\n\"zzz\",\"yyy\",\"xxx\""))))


(deftest escape-crlf
  (is (= [{"aaa" "zzz", "b\r\nbb" "yyy", "ccc" "xxx"}]
         (csv/decode "\"aaa\",\"b\r\nbb\",\"ccc\"\r\nzzz,yyy,xxx"))))


(deftest double-quotes-escaping
  (is (= [{"aaa" "", "b\"bb" "", "ccc" ""}]
         (csv/decode "\"aaa\",\"b\"\"bb\",\"ccc\"\r\n,,"))))


(deftest dupe-keys-unsupported
  (let [csv     (str "field_name,field_name,field_name\r\n"
                     "aaa,bbb,ccc\r\n"
                     "zzz,yyy,xxx\r\n")
        [m1 m2] (csv/decode csv)]
    (is (= ["field_name"] (keys m1)))
    (is (= ["field_name"] (keys m2)))))
