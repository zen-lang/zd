(ns zd.fs
  (:require [zen.core :as zen]
            [clj-jgit.porcelain :as git]))


(defmethod zen/op 'zd/gitsync-last-updated
  [ztx config fp & args]
  (or (when-let [r (->> [:zen/state :gitsync :state :remote :repo]
                        (get-in @ztx))]
        (when-let [l (first (git/git-log r :paths fp :max-count 1))]
          (let [d (get-in l [:author :date])]
            (str (+ 1 (.getMonth d)) "/" (.getDate d) "/" (+ 1900 (.getYear d))))))
      ""))

