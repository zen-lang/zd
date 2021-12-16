(ns zd.deep-merge)

(defn deep-merge
  "efficient deep merge"
  ([a b & more]
   (apply deep-merge (deep-merge a b) more))
  ([a b]
   (if (and (map? a) (map? b))
     (loop [[[k v :as i] & ks] b
            acc a]
       (if (nil? i)
         acc
         (let [av (get a k)]
           (if (= v av)
             (recur ks acc)
             (recur ks
                    (cond
                      (and (map? v) (map? av)) (assoc acc k (deep-merge av v))
                      (and (nil? v) (map? av)) (assoc acc k av)
                      :else (assoc acc k v)))))))
     (do
       (println :error "deep-merge type missmatch: " a b)
       b))))
