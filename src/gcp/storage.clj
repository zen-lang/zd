(ns gcp.storage
  (:require
   [cheshire.core]
   [clojure.string :as str])
  (:import (java.security Signature
                          KeyFactory
                          MessageDigest)
           (java.security.spec PKCS8EncodedKeySpec)
           (java.time ZonedDateTime
                      ZoneOffset)
           java.util.Base64
           org.apache.commons.codec.binary.Hex
           java.net.URLEncoder
           (java.time.format DateTimeFormatter)))

(def ^:private timestamp-fmt (DateTimeFormatter/ofPattern "yyyyMMdd'T'HHmmss'Z'"))
(def ^:private datestamp-fmt (DateTimeFormatter/ofPattern "yyyyMMdd"))

(defn get-sa []
  (cheshire.core/parse-string (slurp "gcp.json") keyword))

(defn- quote-url
  "Replace special characters in string using the %xx escape. Letters, digits, and the characters '_.-' are never quoted. By default, this function is intended for quoting the path section of the URL. The optional safe parameter specifies additional characters that should not be quoted — its default value is '/'.
  Translation of https://docs.python.org/2/library/urllib.html#urllib.quote"
  [^String s & {:keys [safe] :or {safe "/"}}]
  (let [not-safe? (-> safe (str "_.-") set complement)]
    (loop [[chars [safe-char & rest-chars]] (split-with not-safe? s)
           r ""]
      (let [r (str r (URLEncoder/encode (str/join chars) "UTF-8") safe-char)]
        (if (seq rest-chars)
          (recur (split-with not-safe? rest-chars) r)
          r)))))

(defn- hex-str [#^bytes b]
  (Hex/encodeHexString b))

(defn- sha-256-str [^String input]
  (->> (.getBytes input "UTF-8")
       (.digest (MessageDigest/getInstance "SHA-256"))
       hex-str))

(defn- signSHA256RSA
  "RSA-SHA256 with PKCS1v15 padding"
  [^String pk ^String input]
  (let [pk (-> (str pk)
               (str/replace #"-----BEGIN PRIVATE KEY-----" "")
               (str/replace #"-----END PRIVATE KEY-----" "")
               (str/replace #"\n" "")
               (->> (.decode (Base64/getDecoder))
                    (new PKCS8EncodedKeySpec)))

        private-signature (doto (Signature/getInstance "SHA256withRSA")
                            (.initSign (.generatePrivate (KeyFactory/getInstance "RSA") pk))
                            (.update (.getBytes input "UTF-8")))]
    (hex-str (.sign private-signature))))

"Tranlation of https://github.com/GoogleCloudPlatform/python-docs-samples/blob/9e5c2fb563f1cd5de76a72c4d8ae57ce884f66bd/storage/signed_urls/generate_signed_urls.py"
(defn generate-signed-url
  [{service-account :account
    bucket-name :bucket
    object-name :object
    http-method :method
    :keys [subresource expiration  query-parameters headers]}]
  (let [escaped-object-name (quote-url object-name :safe "/~")
        canonical-uri       (str \/ escaped-object-name)

        datetime-now      (ZonedDateTime/now (ZoneOffset/UTC))
        request-timestamp (.format timestamp-fmt datetime-now)
        datestamp         (.format datestamp-fmt datetime-now)

        client-email     (:client_email service-account)
        credential-scope (str datestamp "/auto/storage/goog4_request")
        credential       (str client-email \/ credential-scope)

        bucket-name (or bucket-name (:bucket service-account))

        host    (str bucket-name ".storage.googleapis.com")
        headers (assoc headers :host host)

        ordered-headers   (sort-by key headers)
        canonical-headers (str (->> ordered-headers
                                    (map (comp (partial str/join \:)
                                            (partial map str/lower-case)
                                            (juxt (comp name key) (comp str val))))
                                    (str/join \newline))
                               \newline)
        signed-headers    (->> ordered-headers
                               (map (comp str/lower-case name key))
                               (str/join \;))

        query-parameters (cond-> (assoc query-parameters
                                        :X-Goog-Algorithm     "GOOG4-RSA-SHA256"
                                        :X-Goog-Credential    credential
                                        :X-Goog-Date          request-timestamp
                                        :X-Goog-Expires       (or expiration "604800")
                                        :X-Goog-SignedHeaders signed-headers)
                           subresource (assoc subresource ""))

        canonical-query-string (->> query-parameters
                                    (sort-by key)
                                    (map (comp (partial str/join \=)
                                            (partial map #(quote-url % :safe ""))
                                            (juxt (comp name key) (comp str val))))
                                    (str/join \&))

        canonical-request      (str/join \newline
                                         [(or http-method "GET")
                                          canonical-uri
                                          canonical-query-string
                                          canonical-headers
                                          signed-headers
                                          "UNSIGNED-PAYLOAD"])
        canonical-request-hash (sha-256-str canonical-request)
        string-to-sign (str/join \newline
                                 ["GOOG4-RSA-SHA256"
                                  request-timestamp
                                  credential-scope
                                  canonical-request-hash])

        signature   (signSHA256RSA (:private_key service-account) string-to-sign)
        signed-url  (str "https://" host canonical-uri \? canonical-query-string "&x-goog-signature=" signature)]
    signed-url))


(comment
  (generate-signed-url {:account  (get-sa) :object  "ups" :method  "PUT"})

  )
