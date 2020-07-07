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
  (reduce (fn [{:keys [prices counts-left]} [item seen]]
            (if-let [{:keys [count price]} (get specials item)]
              {:counts-left (merge counts-left {item (rem seen count)})
               :prices      (+ prices (* (quot seen count) price))}
              {:counts-left (merge counts-left {item seen})
               :prices      prices}))
          {:counts-left nil :prices 0}
          counts))


(def count-seen frequencies)


(defn valid-special-list? [specials]
  (every? #(pos? (:count %)) (vals specials)))


(defn total [item-list price-list specials]
  (when-not (valid-special-list? specials)
    (throw (ex-info "invalid special list" {:invalid-special-list true})))
  (let [counts (count-seen item-list)
        {:keys [counts-left prices]} (apply-specials counts specials)]
    (+ prices (apply-regular counts-left price-list))))
