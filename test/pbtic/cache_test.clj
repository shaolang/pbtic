(ns pbtic.cache-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [pbtic.cache :as c]
            [stateful-check.core :refer [specification-correct?]]))

(def cache-size 10)
(def known-keys [:hello :hi :konichiwa])
(def gen-key (gen/one-of [(gen/elements known-keys) gen/keyword]))
(def gen-val gen/nat)

(def cache-command
  {:command     c/cache!
   :args        (fn [_] [gen-key gen-val])
   :next-state  (fn [state [k v] _]
                  (let [entry   {:k k :v v}
                        [x y]   (split-with #(not= k (:k %)) state)
                        nstate  (if (empty y)
                                  (conj state entry)
                                  (vec (concat x [entry] (next y))))]
                    (if (> (count nstate) cache-size)
                      (vec (next nstate))
                      nstate)))})

(def find-command
  {:command       c/find
   :args          (fn [_] [gen-key])
   :postcondition (fn [state _ [k] result]
                    (= result (:v (some #(when (= k (:k %)) %) state))))})

(def flush-command
  {:command       c/flush!
   :requires      (fn [state] (pos? (count state)))
   :next-state    (constantly [])
   :postcondition (fn [_ next-state _ _]
                    (zero? (count next-state)))})

(def cache-specification
  {:commands          {:flush!    #'flush-command
                       :find      #'find-command
                       :cache!    #'cache-command}
   :generate-command  (fn [_] (gen/frequency [[1 (gen/return :cache!)]
                                              [3 (gen/return :find)]
                                              [1 (gen/return :flush!)]]))
   :initial-state     (constantly [])
   :setup             #(c/init! cache-size)})

(deftest cache-specification-test
  (is (specification-correct? cache-specification)))


(deftest cache-specification-parallel-test
  (is (specification-correct? cache-specification {:gen {:threads 2}
                                                   :max {:tries 100}})))
