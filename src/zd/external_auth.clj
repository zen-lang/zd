(ns zd.external-auth
  (:require
   [clojure.string :as str]
   [cheshire.core :as json]
   [ring.util.codec :as codec]
   [clj-http.client :as client]
   [garden.core]
   [clojure.walk]
   [clojure.set]
   [hiccup.core :as hiccup]
   [stylo.core :refer [c c?]]
   [zd.jwt :as jwt])
  (:import java.util.Base64
           java.net.URLEncoder))


(defn to-html [x] (hiccup/html x))

(defn form-encode [m]
  (ring.util.codec/form-encode m))

(defn decode64 [to-decode]
  (->> to-decode
       (.decode (java.util.Base64/getDecoder))
       String.))

(defn encode64 [s]
  (when-not (str/blank? (str s))
    (.encodeToString (java.util.Base64/getEncoder) (if (string? s) (.getBytes s) s))))

(defn parse-params [params]
  (let [params (codec/form-decode params "UTF-8")]
    (if (map? params)
      (reduce-kv (fn [acc k v] (assoc acc (keyword k) v))
                 {}
                 params)
      {})))

(def provider-settings
  {:google {:scopes         ["https://www.googleapis.com/auth/userinfo.profile" "https://www.googleapis.com/auth/userinfo.email"]
            :userinfo_endpoint   "https://www.googleapis.com/oauth2/v1/userinfo"
            :token_endpoint      "https://www.googleapis.com/oauth2/v4/token"
            :authorize_endpoint       "https://accounts.google.com/o/oauth2/v2/auth"
            :name "Google"
            :href "/auth/google"
            :system "https://google.com"
            :toScim {:email [:email]
                     :name [:name :formatted]
                     :given_name [:name :givenName]
                     :family_name [:name :familyName]
                     :link [:profileUrl]
                     :picture [:photo]
                     :locale [:locale]
                     :id (fn [profile] {:identifier [{:system "https://google.com" :value (str (:id profile))}]})
                     :gender [:gender]}}

   :github {:scopes         ["user" "read:org"]
            :name "GitHub"
            :href "/auth/github"
            :system "https://github.com"
            :userinfo_endpoint   "https://api.github.com/user"
            :user_email_endpoint "https://api.github.com/user/emails"
            :token_endpoint      "https://github.com/login/oauth/access_token"
            :authorize_endpoint  "https://github.com/login/oauth/authorize"
            :toScim {:email [:email]
                     :name [:name :formatted]
                     :html_url [:profileUrl]
                     :avatar_url [:photo]
                     :login [:userName]
                     :bio   [:title]
                     :id (fn [profile] {:identifier [{:system "https://github.com" :value (str (:id profile))}]})
                     :location (fn [profile] {:addresses [{:formatted (:location profile)}]})}}})

(defn body
  [msg]
  (to-html
   [:head
    [:style (stylo.core/compile-styles @stylo.core/styles)]
    [:link  {:href "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"  :rel "stylesheet"}]
    [:meta {:charset "UTF-8"}]
    [:body
     [:div {:class (c [:pt 10])}
      [:center [:img {:src "/logo.png" :class (c [:h 20])}]]
      msg]]]))

(defn error-page [msg]
  (body [:div {:class (c [:w 220] [:bg :white]
                         [:mt 20]
                         [:p 5]
                         :text-lg
                         :shadow-xl :border
                         :mx-auto)} msg]))

(defn auth-page
  [opt
   {qs :query-string url :uri :as _req}]
  (let [prov-opts (select-keys opt [:github :google])]
    (body
     [:div {:class (c [:w 120] [:bg :white]
                      [:mt 20]
                      [:rounded :lg]
                      :shadow-xl :border
                      :divide-y
                      :mx-auto)}
      (for [[provider {:keys [organizations organizations-notice]}] prov-opts
            :let [pr (get provider-settings provider)]]
        [:a {:class (c :flex :items-center
                       [:p 6]
                       :cursor-pointer
                       [:space-x 6]
                       [:hover [:bg :gray-200]])
             :href (str (:href pr) "?" qs)}
         [:i.fab {:class [(c {:font-size "32px"}) (str "fa-" (name provider))]}]
         [:div {:class (c :flex-1 :text-xl)} (str "Sign in with " (or (:name pr)
                                                                      (name provider)))
          (when (or organizations organizations-notice)
            [:div {:class (c :text-sm)}
             [:span {:class (c [:text :gray-700])} "You should be part of "]
             [:span {:class (c [:text :gray-700] :font-bold)} (str/join "," (or organizations organizations-notice) )]
             [:span {:class (c [:text :gray-700])} " organization."]])]
         [:div {:class (c [:text :gray-400] {:font-size "32px"})} "›"]])
      ])
    ))

(defn redirect-to-provider [uri]
  {:status 302
   :headers {"location" uri
             "cache-control" "no-cache, no-store, max-age=0, must-revalidate"
             "pragma" "no-cache"}
   :stop true})

(defn redirect-to-auth [{qs :query-string url :uri :as _req}]
  (let [state (encode64 (str url (when-not (empty? qs) (str "?" qs))))]
    {:status 302
     :headers {"location" (str "/auth?state=" state)
               "cache-control" "no-cache, no-store, max-age=0, must-revalidate"
               "pragma" "no-cache"}
     :stop true}))

(defn parse-and-verify-jwt-if-exist
  [{secret :oauth-secret :as _opts} {:keys [cookies] :as req}]
  (when-let [token (get-in cookies ["token" :value])]
    (let [jwt (try (jwt/parse token)
                   (catch Exception _e
                     (throw (ex-info "Wrong token" {:status 403} _e))))]
      (when (jwt/verify jwt secret)
        (:claims jwt)))))

(defn successful-redirect
  [{secret :oauth-secret :as _opts} req userinfo url]
  {:status 302
   :headers {"location" url}
   :cookies {"token" {:value (jwt/sign secret userinfo :HS256)
                      :max-age 31536000
                      :path "/"}}
   :stop true})

(defn get-authorize-url [{bu :base_url ae :authorize_endpoint :as prov}]
  (if (and bu (not (str/starts-with? ae "http")))
    (str bu ae)
    ae))



(defn auth-redirect-uri [url & [params hash]]
  (let [qparams (and params (seq params) (form-encode params))
        hparams (and hash (seq hash) (form-encode hash))]
    (str url
         (when qparams (str "?" qparams))
         (when hparams (str "#" hparams)))))

(defn cb-url [{prov :provider base-uri :base-uri} req]
  (str base-uri (get-in prov [:redirect-uri])))


(defn redirect-provider
  [{prov :provider :as opts}
   {qs :query-string :as req}]
  (let [auth-base-uri (get-authorize-url prov)
        state (-> (parse-params qs) :state)
        params
        (cond->
            {:response_type "code"
             :scope (str/join " " (concat (:scopes prov) (:additional-scopes prov)))
             :client_id (get-in prov [:client-id])
             :redirect_uri  (cb-url opts req)
             :state state})
        response
        (->> params
             (auth-redirect-uri auth-base-uri)
             redirect-to-provider)]
    response))


(defn get-token
  [{{:keys [client-id client-secret token_endpoint] :as prov} :provider :as opts}
   {qs :query-string :as req}]
  (let [{:keys [code state]} (parse-params qs)

        prms {:client_id client-id
              :client_secret client-secret
              :redirect_uri  (cb-url opts req)
              :grant_type    "authorization_code"
              :code code}

        req  {:accept :json
              :form-params prms
              :raise false
              :content-type :x-www-form-urlencoded}

        resp (client/post token_endpoint req)]
    (if (> (:status resp) 399)
      (throw (ex-info "Couldn't get authorization token" {:status 403}))
      {:state  (when state (decode64 state))
       :token (json/parse-string (:body resp) keyword)})))

(defn get-userinfo
  [{{:keys [userinfo_endpoint userinfo_header user_email_endpoint] :as prov} :provider
     {token :access_token :as token-response} :token}]
  (let [userinfo-url userinfo_endpoint
        req {:accept :json
             :headers {"Authorization"
                       (cond
                         userinfo_header (str userinfo_header " " token)
                         :else (str "Bearer " token))}}
        resp (try
               (client/get userinfo-url req)
               (catch Exception e
                 (throw (ex-info "Couldn't get Userinfo" {:status 403} e))))

        email-req {:accept :json
                   :headers {"Authorization" (str "token " token)}}
        email-resp (when userinfo-url
                     (try (client/get user_email_endpoint email-req)
                          (catch Exception _ nil)))
        email-info (when email-resp
                     (try (json/parse-string (:body email-resp) keyword)
                          (catch Exception _ nil)))
        primary-email (:email (first (filter #(:primary %) email-info)))

        userinfo (json/parse-string (:body resp) keyword)
        userinfo (cond-> userinfo
                   email-info (assoc :email-info email-info)

                   (and primary-email
                        (not (:email userinfo)))
                   (assoc :email primary-email))]
    (if (> (:status resp) 399)
      (throw (ex-info "Couldn't get Userinfo" {:status 403}))
      userinfo)))

(defn check-organizations [{{sys :system :as prov} :provider
                            {token :access_token :as token-response} :token
                            userinfo :userinfo}]
  (when-let [allowed-orgs (:organizations prov)]
    (let [req {:accept :json
               :headers {"Authorization" (str "Bearer " token)}}
          ;; TODO: respect pagination
          orgs (when (= "https://github.com" (:system prov))
                 (try
                   (let [resp (client/get "https://api.github.com/user/orgs" req)]
                     (->>
                      (json/parse-string (:body resp ) keyword)
                      (mapv :login)
                      (filterv identity)
                      (into #{})))
                   (catch Exception e
                     (throw (ex-info "Couldn't get organizations" {:status 403} e)))))
          orgs (if-let [e (:email userinfo)]
                 (if-let [o (second (str/split e #"@" 2))]
                   (conj orgs o)
                   orgs)
                 orgs)]

      (when (empty? (clojure.set/intersection (into #{} allowed-orgs)
                                              (into #{} (or orgs []))))
        {:status 403
         :stop true
         :body (error-page (str "You should be member of [" (str/join "," allowed-orgs)
                                "] organizations. But only [" (str/join "," orgs) "]"))}))))

(defn callback [opts req]
  (let [{:keys [token state] } (get-token opts req)
        userinfo (get-userinfo (assoc opts :token token))
        org-error (check-organizations (assoc opts
                                              :token token
                                              :userinfo userinfo))]
    (if org-error
      org-error
      (successful-redirect opts req (-> userinfo
                                        (select-keys [:email :name :url])
                                        (assoc :token token))
                           state))))


(defn white-list? [ztx opt {:keys [uri] :as req}]
  (let [uris #{"/logo.png" "/favicon.ico"}]
    (uris uri)))

(defn dispatch
  [ztx opt {:keys [uri] :as req}]
  (let [user (parse-and-verify-jwt-if-exist opt req)]
    (cond
      user (assoc req :user user)

      (white-list? ztx opt req) req

      (= uri "/auth") {:status 200
                       :body  (auth-page opt req)
                       :stop true}

      :else
      (try
        (let [parts (->> (str/split uri #"/") (remove empty?))
              provider (#{:github :google} (-> parts last keyword))
              opt' (assoc opt :provider (merge (get provider-settings provider)
                                               (get opt provider)))]
          (cond
            (= ["auth" "callback"] (take 2 parts)) (callback opt' req)
            (= ["auth"] (take 1 parts)) (redirect-provider opt' req)
            :else (redirect-to-auth req)))
        (catch clojure.lang.ExceptionInfo e
          {:body (error-page (ex-message e))
           :status  (:status (or (ex-data e) 500))
           :stop true})))))


(defn auth [ztx opts]
  (fn [cb]
    (fn [req]
      (let [{stop :stop :as res} (dispatch ztx opts req)]
        (if stop
          (dissoc res :stop)
          (cb res))))))
