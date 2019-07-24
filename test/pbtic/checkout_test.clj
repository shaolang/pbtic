(ns pbtic.checkout-test
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :refer [for-all]]
            [pbtic.checkout :as checkout]))

;;;;;;;;;;;;;
;; generators

(def gen-non-empty-string
  (let [chars "abcdefghijklmnopqrstuvwxyz0123456789"]
    (gen/let [cs (gen/list (gen/elements chars))]
      (apply str (rand-nth chars) cs))))


(def gen-non-neg-int (gen/fmap inc gen/nat))


(def gen-price-list ;; gens {<item> => <price>}
  (gen/let [price-list (gen/not-empty
                        (gen/list (gen/tuple gen-non-empty-string gen/nat)))]
    (into {} price-list)))


(defn gen-item-list [price-list]  ;; gens [[<item>s] {<item> => <price>}]
  (gen/let [items (gen/list (gen/elements (keys price-list)))]
    [items
     (->> items
          (map #(get price-list %))
          (reduce + 0))]))


(def gen-item-price-list
  (gen/let [prices                  gen-price-list
            [items expected-price]  (gen-item-list prices)]
    {:items           items
     :expected-price  expected-price
     :prices          prices}))


(defn gen-special-list [prices] ;; gens {<item> => {:count <n> :price <n>}}
  (gen/let [specials (gen/list (gen/tuple (gen/elements (keys prices))
                                          (gen/choose 2 5)
                                          gen/nat))]
    (apply merge
           (map (fn [[item count price]] {item {:count count :price price}})
                specials))))


(defn gen-regular [prices specials]
  (letfn [(gen-count [item]
            (if (contains? specials item)
              (gen/choose 0 (dec (get-in specials [item :count])))
              gen/nat))]
    (gen/return
      (->> prices
           (map (fn [[item price]]
                  (let [counts (gen/generate (gen-count item))]
                    {:items     (repeat counts item)
                     :sub-total (* counts price)})))
           (reduce (fn [{:keys [all-items total] :as result}
                        {:keys [items sub-total]}]
                     (assoc result
                            :total (+ total sub-total)
                            :all-items (shuffle (concat all-items items))))
                   {:total 0
                    :all-items []})
           ((juxt :all-items :total))))))


(defn gen-special [_ specials]
  (gen/let [multipliers (gen/vector gen/nat (count specials))]
    (->> (zipmap specials multipliers)
         (map (fn [[[item {:keys [count price]}] multiplier]]
                {:items (repeat (* count multiplier) item)
                 :sub-total (* price multiplier)}))
         (reduce (fn [{:keys [all-items total] :as result}
                      {:keys [items sub-total]}]
                   (assoc result
                          :all-items (concat all-items items)
                          :total     (+ total sub-total)))
                 {:total 0
                  :all-items []})
         ((juxt :all-items :total)))))


(def gen-item-price-special
  (gen/let [prices    gen-price-list
            specials  (gen-special-list prices)
            [spec-items spec-price] (gen-special prices specials)
            [reg-items reg-price]   (gen-regular prices specials)]
    {:items           (shuffle (concat spec-items reg-items))
     :expected-price  (+ spec-price reg-price)
     :prices          prices
     :specials        specials}))


(def gen-lax-lists
  (gen/hash-map
   :items   (gen/list gen/string-alphanumeric)
   :prices  (gen/fmap #(into {} %) (gen/list (gen/tuple gen/string-alphanumeric
                                                        gen/nat)))
   :specials (gen/fmap (fn [vs]
                        (->> vs
                             (map (fn [[item count price]] {item {:count count
                                                                  :price price}}))
                             (apply merge)))
                       (gen/list (gen/tuple gen/string-alphanumeric
                                            gen/nat
                                            gen/nat)))))

;;;;;;;;;;;;;
;; propreties

(defspec sums-without-specials
  (for-all [{:keys [items expected-price prices]} gen-item-price-list]
    (= expected-price
       (checkout/total items prices {}))))


(defspec sums-with-specials
  (for-all [{:keys [items expected-price prices specials]} gen-item-price-special]
    (= expected-price
       (checkout/total items prices specials))))


(defspec negative-testing-for-expected-results
  (for-all [{:keys [items prices specials]} gen-lax-lists]
    (try
      (integer? (checkout/total items prices specials))
      (catch Throwable ex
        (contains? (ex-data ex) :unknown-item)))))
