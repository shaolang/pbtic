(ns pbtic.birthday.bday-filter-test
  (:require [pbtic.birthday.bday-filter :as filter :refer [month-day]]
            [clojure.set :as set]
            [clojure.test :refer [deftest is]])
  (:import [java.time DateTimeException LocalDate]))

;;;;;;;;;;
;; helpers

(defn find-birthdays-for-year [people yeardata]
  (when (seq yeardata)
    (let [[day & year]  yeardata
          found         (filter/birthday people day)]   ;; <- function being tested
      (assoc (find-birthdays-for-year people year) day found))))


(defn generate-year-data [start]
  (let [start-date  (LocalDate/of start 1 1)
        end-date    (LocalDate/of (inc start) 1 1)]
    (into [] (.. start-date (datesUntil end-date) toArray))))


(defn generate-years-data [start end]
  (mapv generate-year-data (range start (inc end))))


(defn rand-name []
  (apply str (repeatedly 30 #(rand-nth "abcdefghijklmnopqrstuvwxyz"))))


(defn people-for-date [date]
  (try
    (let [[month day] (month-day date)
          rand-year   (+ 1900 (rand-int 100))]
      {:name          (rand-name)
       :date-of-birth (LocalDate/of rand-year month day)})
    (catch Exception _ (people-for-date date))))


(defn people-for-year [year]
  (map people-for-date year))


(defn generate-people-for-year [n]
  (let [year-seed (generate-year-data 2016)]  ;; leap year so all days are covered
    (mapcat (fn [_] (people-for-year year-seed)) (range n))))

;;;;;;;;;;;;;
;; assertions

(defn every-birthday-once [people birthdays]
  (let [found       (mapcat second birthdays)
        not-found   (set/difference (set people) (set found))]
    (is (empty? not-found))
    (is (zero? (- (count found) (count (set found)))))))


(defn on-right-date [people birthdays]
  (doseq [[date found]            birthdays
          {:keys [date-of-birth]} found]
    (let [[dob-month dob-day] (month-day date-of-birth)]
      (try
        (LocalDate/of (.getYear date) dob-month dob-day)
        (is (= (month-day date)
               (month-day date-of-birth)))
        (catch DateTimeException _ true)))))

;;;;;;;
;; test

(deftest property-style-filtering
  (let [years   (generate-years-data 2018 2038)
        people  (generate-people-for-year 3)]
    (doseq [yeardata years]
      (let [birthdays (find-birthdays-for-year people yeardata)]
        (every-birthday-once people birthdays)
        (on-right-date people birthdays)))))
