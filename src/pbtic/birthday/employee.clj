(ns pbtic.birthday.employee
  (:require [clojure.string :as str]
            [pbtic.birthday.bday-filter :as bday-filter]
            [pbtic.birthday.csv :as csv])
  (:import [java.time LocalDate]
           [java.time.format DateTimeFormatter]))

;;;;;;;
;; defs

(def ^:private datetime-formatter (DateTimeFormatter/ofPattern "yyyy/MM/dd"))

;;;;;;;;;;;;;
;; public API

(defn adapt-csv-result [m]
  (let [ks  (sequence (comp (map str/triml)
                            (map #(str/replace % #"_" "-"))
                            (map keyword))
                      (keys m))
        m   (zipmap ks (map str/triml (vals m)))
        dob (:date-of-birth m)]
    (assoc m :date-of-birth (LocalDate/parse dob datetime-formatter))))


(defn from-csv [s]
  (map adapt-csv-result (csv/decode s)))


;; parameter order is different from the book's
(defn filter-birthday [date employees]
  (bday-filter/birthday employees date))
