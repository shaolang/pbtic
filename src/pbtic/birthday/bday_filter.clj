(ns pbtic.birthday.bday-filter)


(def month-day (juxt #(.getMonthValue %) #(.getDayOfMonth %)))


(defn birthday-no-leap-year-handling [people date]
  (let [md (month-day date)]
    (filter #(= (month-day (:date-of-birth %)) md) people)))


(defn  filter-dob [people month day]
  (filter #(= (month-day (:date-of-birth %)) [month day]) people))


(defn birthday [people date]
  (let [[month day] (month-day date)]
    (if (and (= [month day] [2 28]) (not (.isLeapYear date)))
      (concat (filter-dob people 2 28) (filter-dob people 2 29))
      (filter-dob people month day))))
