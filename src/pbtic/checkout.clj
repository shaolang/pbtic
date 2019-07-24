(ns pbtic.checkout)


(defn cost-of-item [prices item]
  (if-let [price (get prices item)]
    price
    (throw (ex-info "unknown-item" {:unknown-item item}))))


(defn apply-regular [items prices]
  (transduce (map (fn [[item count]] (* count (cost-of-item prices item))))
             +
             0
             items))


(defn apply-specials [counts specials]
  (let [counts-map (apply merge (map (fn [[item seen]] {item {:seen seen}})
                                     counts))]
    (->> (merge-with merge specials counts-map)
         (map (fn [[item {:keys [count price seen]}]]
                (when seen
                  {:item      item
                   :remainder (if count (rem seen count) seen)
                   :price     (if price (* (quot seen count) price) 0)})))
         (remove nil?)
         (reduce (fn [result {:keys [item remainder price]}]
                   (-> result
                       (update :counts-left merge {item remainder})
                       (update :prices + price)))
                 {:counts-left nil :prices 0}))))


(def count-seen frequencies)


(defn total [item-list price-list specials]
  (let [counts (count-seen item-list)
        {:keys [counts-left prices]} (apply-specials counts specials)]
    (+ prices (apply-regular counts-left price-list))))
