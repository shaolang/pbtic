(ns pbtic.birthday
  (:require [pbtic.birthday.csv :as csv]
            [pbtic.birthday.employee :as employee]
            [pbtic.birthday.mail-tpl :as mail-tpl])
  (:import [java.time LocalDate]))

;;;;;;;;;
;; helper

(defn- send-email [[to, _topic, _body]]
  (println "sent birthday email to" to))

;;;;;;;;;;;;;
;; public api

(defn run [path & {:keys [curr-date] :or {curr-date (LocalDate/now)}}]
  (doseq [employee (->> (slurp path)
                        employee/from-csv
                        (employee/filter-birthday curr-date))]
    (send-email (mail-tpl/full employee))))
