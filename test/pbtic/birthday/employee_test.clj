(ns pbtic.birthday.employee-test
  (:require [clojure.string :as str]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :refer [for-all]]
            [pbtic.birthday.csv-test :as csv-test]
            [pbtic.birthday.employee :as employee])
  (:import [java.time LocalDate]))

;;;;;;;
;; defs

(def start-date (LocalDate/of 1900 1 1))

(def max-days (.. start-date
                  (until (LocalDate/of 2021 1 1))
                  getDays))

;;;;;;;;;;;;;
;; generators

(def text-date
  (gen/let [days-to-add (gen/choose 0 max-days)]
    (let [date (.plusDays start-date days-to-add)]
      (format " %4d/%02d/%02d"
              (.getYear date)
              (.getMonthValue date)
              (.getDayOfMonth date)))))


(def whitespaced-text
  (gen/let [txt csv-test/field]
    (str " " txt)))


(def raw-employee-map
  (gen/let [val-list (gen/tuple csv-test/field
                                whitespaced-text
                                text-date
                                whitespaced-text)]
    (zipmap ["last_name", " first_name", " date_of_birth", " email"] val-list)))

;;;;;;;;;;;;;
;; properties

(defspec check-that-leading-space-is-fixed
  (for-all [m raw-employee-map]
    (let [emp (employee/adapt-csv-result m)]
      (every? #(not (str/starts-with? (name %) " "))
              (concat (keys emp)
                      (filter string? (vals emp)))))))


(defspec check-that-date-is-formatted-right
  (for-all [m raw-employee-map]
    (let [m (employee/adapt-csv-result m)]
      (= (type (get m :date-of-birth)) LocalDate))))
