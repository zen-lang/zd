(ns zd.runner
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.lang ProcessBuilder])
  (:gen-class))

(defn read-env  [override]
  (->
   (->> (System/getenv)
        (reduce (fn [acc [k v]]
                  (assoc acc (keyword k) v)) {}))
   (merge override)))

(defn read-stream [s]
  (let [r (io/reader s)]
    (loop [acc []]
      (if-let [l (.readLine r)]
        (recur (conj acc l))
        acc))))

(defn proc [{dir :dir env :env args :exec}]
  (let [proc (ProcessBuilder. (into-array String args))
        _ (when dir (.directory proc (io/file dir)))
        _ (when env
            (let [e (.environment proc)]
              #_(.clear e)
              (doseq [[k v] env]
                (.put e (name k) (str v)))))]
    proc))

(defn exec [{dir :dir env :env args :exec :as opts}]
  (let [prc (proc opts)
        p (.start prc)]
    (.waitFor p)
    {:status (.exitValue p)
     :stdout (read-stream (.getInputStream p))
     :stderr (read-stream (.getErrorStream p))}))

(defn run [opts]
  (let [prc (proc opts)]
    (.start prc)))

(defn init-env [{gh-key :ZO_GH_KEY dir :ZO_DIR repo :ZO_REPO :as opts}]
  {:GIT_SSH_COMMAND (format "ssh -i %s -o IdentitiesOnly=yes -o StrictHostKeyChecking=no" gh-key)})

(defn init-repo [{app :ZO_APP gh-key :ZO_GH_KEY dir :ZO_DIR repo :ZO_REPO :as opts}]
  (assert dir "ZO_DIR")
  (assert repo "ZO_REPO")
  (assert gh-key "ZO_GH_KEY")
  (assert app "ZO_APP")
  (assert (.exists (io/file gh-key)))
  (let [env (init-env opts)
        _ (println :env env)
        res (exec {:env env
                   :exec ["git" "clone" repo dir]
                   :dir "/tmp"})]
    (println res)))

(defn current-commit [{dir :ZO_DIR :as opts}]
  (let [env (init-env opts)]
    (-> (exec {:exec ["git" "rev-parse" "HEAD"] :env env :dir dir})
        (get-in [:stdout 0]))))

(defn start-process [opts]
  (println "START PROCESSS!" (str/split (:ZO_APP opts) #"\s+"))
  (let [dir (:ZO_DIR opts)
        env (init-env opts)
        args (str/split (:ZO_APP opts) #"\s+")]
    (exec {:exec ["git" "submodule" "init"] :env env :dir dir})
    (exec {:exec ["git" "submodule" "update"] :env env :dir dir})
    (let [prc (proc {:exec args :dir dir})]
      (.inheritIO prc)
      (.start prc))))

(defn start [{port :PORT gh-key :ZO_GH_KEY dir :ZO_DIR repo :ZO_REPO timeout :ZO_TIMEOUT :as opts}]
  (let [timeout (if timeout (Integer/parseInt timeout) 10000)
        dir (io/file dir)
        env (init-env opts)]
    (println (exec {:exec ["cp" gh-key "/tmp/gh-key"]}))
    (println (exec {:exec ["chmod" "400" "/tmp/gh-key"]}))
    (println (exec {:exec ["ls" "-lah" "/tmp/gh-key"]}))
    (let [opts (assoc opts :ZO_GH_KEY "/tmp/gh-key")
          env (init-env opts)]
      (when-not (.exists dir) (init-repo opts))
      (prn "Repo inited.")
      (loop [version (current-commit opts)
             p (start-process opts)]
        (exec {:exec ["git" "pull" "--rebase"] :env env :dir dir})
        (print "*") (flush)
        (let [test-version (current-commit opts)]
          (if (= test-version version)
            (do
              (Thread/sleep timeout)
              (recur version p))
            (do
              (println :restart-app test-version)
              (println "DESTROY" (.destroy p))
              #_(println  (exec {:exec ["kill" "-9" (str (.pid p))]
                               :dir "/"}))
              (let [p (start-process opts)]
                (println :start p)
                (Thread/sleep timeout)
                (recur test-version p)))))))))

(defn -main [& _]
  (start (read-env {:ZO_GH_KEY "./keys/knowledge-base-git"
                    :ZO_DIR "/tmp/knowledge-base"
                    :ZO_APP "clojure -M:prod"
                    :ZO_REPO "git@github.com:HealthSamurai/knowledge-base.git"})))

(comment

  (def start-env
    {:ZO_GH_KEY "./keys/knowledge-base-git"
     :ZO_DIR "/tmp/knowledge-base"
     :ZO_APP "bb run"
     :ZO_REPO "git@github.com:HealthSamurai/knowledge-base.git"})


  (def t (Thread. (fn [] (start start-env))))

  (.start t)

  (.stop t)

  (.isAlive p)

  (def out (io/reader (.getInputStream p)))

  (.exitValue p)

  (read-stream (.getErrorStream p))

  (.readLine out)
  (.destroy p)

  (read-env {})

  (exec {:exec ["cat" "creds"]
         :dir "/gcp"})

  )
