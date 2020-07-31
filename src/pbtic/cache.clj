(ns pbtic.cache)

(def ^:private *ets (atom nil))

(defn init! [max-items]
  (reset! *ets {:index 0 :max-items max-items}))


(defn- find* [k]
  (when-let [[rec] (filter (fn [[_index tuple]]
                             (and (vector? tuple) (= k (first tuple))))
                           @*ets)]
    rec))


(defn find [k]
  (when-let [[_ [_ v]] (find* k)]
    v))


(defn cache [k v]
  (if-let [[i] (find* k)]
    (swap! *ets assoc i [k v])
    (let [new-index (if (= (:index @*ets) (:max-items @*ets))
                      1
                      (inc (:index @*ets)))]
      (swap! *ets assoc new-index [k v] :index new-index))))


(defn flush []
  (let [{:keys [max-items]} @*ets]
    (init! max-items)))
