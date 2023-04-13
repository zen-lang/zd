(ns zd.gitsync
  (:require [zen.core :as zen]
            [clojure.string :as str]
            [zd.utils :as utils]
            [zd.loader :as loader]
            [clojure.java.io :as io]
            [clj-jgit.porcelain :as git])
  (:import [java.util Timer TimerTask]))

(defonce ti (Timer.))

(defonce ag (agent nil))

(comment
  (def ag (agent nil))

  @ag

  (agent-error ag)

  (restart-agent ag nil)

  )

(defn commit-doc [_ ztx config repo {p :docpath d :docname}]
  (let [;; TODO sync all untracked docs at gitsync start?
        {:keys [untracked modified] :as status} (git/git-status repo)
        git-config (git/git-config-load repo)]
    (doseq [m (into untracked modified)]
      (when (str/includes? p m)
        (let [uname (.getString git-config "user" nil "name")
              email (.getString git-config "user" nil "email")]
          (git/git-add repo m)
          (let [msg (if (contains? untracked m)
                      (str "Create " d)
                      (str "Edit " d))]
            (git/git-commit repo msg :committer {:name uname :email email}))
          (git/git-push repo))))))

(defn delete-doc [_ ztx config repo {p :docpath d :docname}]
  (let [{:keys [missing]} (git/git-status repo)
        git-config (git/git-config-load repo)
        uname (.getString git-config "user" nil "name")
        email (.getString git-config "user" nil "email")]
    (doseq [m missing]
      (when (str/includes? p m)
        (git/git-rm repo m)
        (git/git-commit repo (str "Delete " d) :committer {:name uname :email email})
        (git/git-push repo)))))

(defn pull-remote [_ ztx config repo]
  (let [pull-result (git/git-pull repo)]
    ;; TODO resolve merge conflicts
    (when (.isSuccessful pull-result)
      (let [updated? (-> (.getFetchResult pull-result)
                         (.getTrackingRefUpdates)
                         (.isEmpty)
                         (not))]
        (when updated?
          (loader/reload! ztx))))))

(defn init-remote [ztx config {:keys [from branch to] :as remote}]
  (let [repo (if (.exists (io/file to))
               (git/load-repo to)
               (git/git-clone from :dir to))]
    ;; TODO add create/checkout default branch if necessary?
    (when branch
      (git/git-checkout repo branch))
    (git/git-pull repo)
    (git/git-submodule-init repo)
    (git/git-submodule-update repo :strategy :recursive)
    repo))

(defn get-repo [ztx]
  (->> [:zen/state :gitsync :state :remote :repo]
       (get-in @ztx)))

(defmethod zen/op 'zd/gitsync-doc
  [ztx config {p :docpath :as params} & args]
  (when-let [repo (get-repo ztx)]
    (println :gitsync-doc p)
    (let [sf (utils/safecall commit-doc {:type :gitsync/put-doc-error})]
      (send-off ag sf ztx config repo params)
      (await ag))))

(defmethod zen/op 'zd/gitsync-delete-doc
  [ztx config {p :docpath :as params} & args]
  (when-let [repo (get-repo ztx)]
    (println :gitsync-remove-doc p)
    (let [sf (utils/safecall delete-doc {:type :gitsync/delete-doc-error})]
      (send-off ag sf ztx config repo params)
      (await ag))))

(defmethod zen/start 'zd/gitsync
  [ztx {:keys [remote pull-rate] :as config} & opts]
  ;; TODO emit zen.event
  (println 'starting-gitsync)
  ;; TODO make it work with remotes seq
  (let [sf (utils/safecall init-remote {:type :gitsync/remote-init-error})
        repo (:result (sf ztx config remote))]
    (if (instance? org.eclipse.jgit.api.Git repo)
      (let [task (proxy [TimerTask] []
                   (run []
                     (let [sf (utils/safecall pull-remote {:type :gitsync/pull-remote-error})]
                       (send-off ag sf ztx config repo))))]
        (.scheduleAtFixedRate ti task pull-rate pull-rate)
        {:ag ag
         :ti ti
         :task task
         :remote (assoc remote :repo repo)})
      ;; TODO implement graceful shutdown? implement auto restart in zen system?
      ;; how do we handle this use case from zen system perspective?
      {:ag ag
       :ti ti})))

(defmethod zen/stop 'zd/gitsync
  [ztx config {r :remote :as state} & opts]
  ;; TODO emit zen.event
  (println 'stopping-gitsync)
  (when (some? (:repo r))
    (.cancel (:task state))
    (.purge (:ti state))))
