(ns zd.fs
  (:require
   [zd.meta :as meta]
   [zd.memstore :as memstore]
   [clojure.string :as str]
   [zd.gitsync :as gitsync]
   [zd.utils :as utils]
   [clojure.java.io :as io]
   [zen.core :as zen])
  (:import [java.util Timer TimerTask]))

(defn get-repo [ztx]
  (->> [:zen/state :zd.fs :state :remote :repo]
       (get-in @ztx)))

(defn get-state [ztx]
  (->> [:zen/state :zd.fs :state]
       (get-in @ztx)))

(defn load-docs! [ztx dirs]
  (doseq [dir dirs]
    (let [dir (io/file dir)
          dir-path (.getPath dir)]
      ;; load metadata
      (doseq [f (->> (file-seq dir)
                     (filter (fn [f] (str/includes? (.getName f) "_schema.zd"))))]
        (let [content (slurp f)]
          (meta/load-meta! ztx {:path (.getPath f)
                                :resource-path (subs (.getPath f) (inc (count dir-path)))
                                :content content})))
      ;; load documents
      (doseq [[path f] (->> (file-seq dir)
                            (map (fn [d] [(.getPath d) d]))
                            (sort-by first))]
        (when (and (str/ends-with? path ".zd")
                   (not (str/starts-with? (.getName f) ".")))
          (let [resource-path (subs path (inc (count dir-path)))
                content (slurp f)]
            (memstore/load-document! ztx {:path path
                                          :resource-path resource-path
                                          :content content})))))))

(defonce ag (agent nil))

(defonce ti (Timer.))

(defn reload [ag ztx paths]
  ((utils/safecall (fn []
                     (println :zd.fs/reload)
                     (swap! ztx dissoc :zdb :zd/meta :zrefs :zd/macros)
                     (load-docs! ztx paths)
                     (memstore/load-links! ztx)
                     (memstore/eval-macros! ztx)
                     ;; TODO think about return value
                     'ok)
                {:type :zd.fs/reload-error})))

(defn delete-doc! [ag ztx filepath]
  ((utils/safecall (fn [] (io/delete-file filepath)) {:type 'zd.fs/delete-doc-error})))

(defn gitsync-delete! [ag ztx repo params]
  ((utils/safecall gitsync/delete-doc {:type :gitsync/delete-doc-error}) ztx repo params))

(defmethod zen/op 'zd.events/fs-delete
  [ztx config {_ev :ev {docname :docname} :params} & [_session]]
  (println :zd.fs/delete docname)
  (let [{pths :paths} (get-state ztx)
        parts (str/split docname #"\.")
        filepath ;; TODO scan all paths?
        (str (first pths)
             "/"
             (str/join "/" parts)
             ".zd")]
    (send-off ag delete-doc! ztx filepath)
    (when-let [repo (get-repo ztx)]
      (send-off ag gitsync-delete! ztx repo {:docpath filepath :docname docname}))
    (send-off ag reload ztx pths)
    (await ag)))

(defn save-doc! [ag dirname filename content]
  ((utils/safecall (fn []
                     (.mkdirs (io/file dirname))
                     (spit filename content))
                   {:type :zd.fs/save-doc-error})))

(defn gitsync-commit! [ag ztx repo params]
  ((utils/safecall gitsync/commit-doc {:type :zd.gitsync/put-doc-error}) ztx repo params))

(defmethod zen/op 'zd.events/fs-save
  [ztx config {_ev :ev {docname :docname cnt :content} :params} & [_session]]
  ;; TODO emit zen event
  (println :zd.fs/save docname)
  (let [{pths :paths} (get-state ztx)
        dirname
        (->> (str/split docname #"\.")
             butlast
             (str/join "/")
             (str (first pths) "/"))
        filename (str (first pths) "/" (str/replace docname "." "/") ".zd")]
    (send-off ag save-doc! dirname filename cnt)
    (when-let [repo (get-repo ztx)]
      (send-off ag gitsync-commit! ztx repo {:docpath filename :docname docname}))
    (send-off ag reload ztx pths)
    (await ag)
    #_(send-off ag reload-single ztx config {:docpath filename :docname docname})))

(defn sync-remote [ag ztx paths repo]
  (let [{st :status}
        ((utils/safecall gitsync/sync-remote {:type :gitsync/pull-remote-error}) ztx repo)]
    (when (= :updated st)
      (reload ag ztx paths))))

(defmethod zen/start 'zd.engines/fs
  [ztx {:keys [remote paths pull-rate] :as config} & args]
  ;; TODO process possible error and shutdown gracefully
  ;; TODO emit zen event
  (println :zd.fs/start)
  (let [repo
        (:result ((utils/safecall gitsync/init-remote {:type :gitsync/remote-init-error}) ztx remote))
        st
        (if (instance? org.eclipse.jgit.api.Git repo)
          (let [task (proxy [TimerTask] []
                       (run []
                         (send-off ag sync-remote ztx paths repo)))]
            (.scheduleAtFixedRate ti task pull-rate pull-rate)
            {:ag ag
             :ti ti
             :paths paths
             :task task
             :remote (assoc remote :repo repo)})
          ;; TODO if no git repo schedule retry
          {:ag ag
           :paths paths
           :ti ti})]
    (send-off ag reload ztx paths)
    (await ag)
    st))

(defmethod zen/stop 'zd.engines/fs
  [ztx config {r :remote :as state} & args]
  ;; TODO emit zen event
  (println :zd.fs/stop)
  (swap! ztx dissoc :zdb :zd/meta :zrefs :zd/macros)
  (when (some? (:repo r))
    (.cancel (:task state))
    (.purge (:ti state))))

(comment
  (def ag (agent nil))

  @ag

  (agent-error ag)

  (restart-agent ag nil))
