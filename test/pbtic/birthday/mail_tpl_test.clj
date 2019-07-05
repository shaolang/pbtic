(ns pbtic.birthday.mail-tpl-test
  (:require [clojure.string :as str]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :refer [for-all]]
            [pbtic.birthday.csv-test :as csv-test]
            [pbtic.birthday.mail-tpl :as mail-tpl])
  (:import [java.time LocalDate]))

;;;;;;;;;;;;;
;; generators

(def date
  (gen/let [days-to-add gen/nat]
   (.plusDays (LocalDate/of 1900 1 1) days-to-add)))


(def employee-map
  (gen/let [vs (gen/tuple (gen/not-empty csv-test/field)
                          (gen/not-empty csv-test/field)
                          date
                          (gen/not-empty csv-test/field))]
    (zipmap [:last-name :first-name :date-of-birth :email] vs)))

;;;;;;;;;;;;;
;; properties

(defspec email-template-has-first-name
  (for-all [employee employee-map]
    (str/includes? (mail-tpl/body employee)
                   (:first-name employee))))
