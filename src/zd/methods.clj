(ns zd.methods)

(defmulti render-key      (fn [ztx {pth :path}] pth))
(defmulti key-data        (fn [ztx pth data] pth))
(defmulti title-actions   (fn [ztx block] :override))
(defmulti render-block    (fn [ztx {{blk :block} :annotations}] (keyword blk)))
(defmulti render-content  (fn [ztx {{cnt :content} :annotations}] (keyword cnt)))
(defmulti annotation      (fn [nm params] (keyword nm)))
(defmulti inline-method   (fn [ztx m arg ctx] (keyword m)))
(defmulti inline-function (fn [ztx m arg ctx] (keyword m)))
(defmulti process-block   (fn [ztx tp args cnt] tp))
