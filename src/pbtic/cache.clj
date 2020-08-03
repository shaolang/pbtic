(ns pbtic.cache
  (:refer-clojure :exclude [find]))

(def ^:private *ets (atom nil))

(defn init! [max-items]
  (reset! *ets {:index 0 :max-items max-items}))


(defn- find* [k]
  (some (fn [[i vs :as entry]] (when (and (vector? vs) (= k (first vs))) entry))
        @*ets))


(defn find [k]
  (when-let [[_ [_ v]] (find* k)]
    v))


(defn cache! [k v]
  (if-let [[i] (find* k)]
    (swap! *ets assoc i [k v])
    (let [new-index (inc (mod (:index @*ets) (:max-items @*ets)))]
      (swap! *ets assoc new-index [k v] :index new-index))))


(defn flush! []
  (let [{:keys [max-items]} @*ets]
    (init! max-items)))
