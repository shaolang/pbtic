(ns pbtic.birthday.csv
  (:require [clojure.data.csv :as csv]))

(defn encode [ms]
  (let [ks    (-> ms first keys)
        vs    (map vals ms)
        out   (java.io.StringWriter.)]
    (csv/write-csv out (conj vs ks) :newline :cr+lf)
    (.toString out)))


(defn decode [s]
  (let [[header & body] (csv/read-csv (java.io.StringReader. s))]
    (map (partial zipmap header) body)))
