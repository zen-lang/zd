(ns zd.jwt
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [org.bouncycastle.openssl PEMParser PEMKeyPair PEMEncryptedKeyPair]
           [org.bouncycastle.openssl.jcajce JcaPEMKeyConverter JcePEMDecryptorProviderBuilder]))


(java.security.Security/addProvider (org.bouncycastle.jce.provider.BouncyCastleProvider.))

(defn crypto-eq?
  "Test whether two sequences of characters or bytes are equal in a way that
  protects against timing attacks. Note that this does not prevent an attacker
  from discovering the *length* of the data being compared."
  [a b]
  (let [a (map int a), b (map int b)]
    (if (and a b (= (count a) (count b)))
      (zero? (reduce bit-or (map bit-xor a b)))
      false)))

(defn decode64 [str]
  (.decode (java.util.Base64/getDecoder) str))

(defn encode64 [str]
  (.encodeToString (java.util.Base64/getEncoder) (if (string? str) (.getBytes str) str)))

(defn safe-encode [s]
  (-> (encode64 s)
      (str/replace #"\s" "")
      (str/replace "=" "")
      (str/replace "+" "-")
      (str/replace "/" "_")))

(defn safe-decode [s]
  (-> (case (mod (count s) 4)
        2 (str s "==")
        3 (str s "=")
        s)
      (str/replace "-" "+")
      (str/replace "_" "/")
      decode64))

(defn ^JcaPEMKeyConverter pem-converter
  [] (JcaPEMKeyConverter.))

(defn get-private-key [key-info _]
  (-> (pem-converter)
      (.getKeyPair key-info)
      .getPrivate))

(defn private-key
  [key-str & [pass-phrase]]
  (with-open [reader (io/reader (.getBytes key-str))]
    (some-> reader
            PEMParser.
            .readObject
            (get-private-key pass-phrase))))

(defn get-public-key
  [key-info _]
  (.getPublicKey (pem-converter) key-info))


(defn public-key
  [key-str & [pass-phrase]]
  (with-open [reader (io/reader (.getBytes key-str))]
    (some-> reader
            PEMParser.
            .readObject
            (get-public-key pass-phrase))))

(def ^:private algs
  {:RS256 "SHA256withRSA"
   :RS384 "SHA384withRSA"
   :RS512 "SHA512withRSA"
   :HS256 "HmacSHA256"
   :HS384 "HmacSHA384"
   :HS512 "HmacSHA512"})

(defn- rsa-sign
  "Function to sign data with RSA algorithm."
  [key body & [alg]]
  (let [sig (doto (java.security.Signature/getInstance alg)
              (.initSign key (java.security.SecureRandom.))
              (.update (.getBytes body "UTF-8")))]
    (safe-encode (.sign sig))))

(defn hmac-sign
  "Function to sign data with HMAC algorithm."
  [key body alg]
  (let [hmac-key (javax.crypto.spec.SecretKeySpec. (.getBytes key "UTF-8") alg)
        hmac     (doto (javax.crypto.Mac/getInstance alg)
                   (.init hmac-key))]
    (safe-encode (.doFinal hmac (.getBytes body "UTF-8")))))

(defn sign [private-key claims & [alg]]
  (let [alg (keyword (or alg :RS256))
        header (safe-encode (json/generate-string {:alg (name alg) :typ "JWT"}))
        claims (safe-encode (json/generate-string claims))
        data (str header "." claims)
        alg-key (get algs alg)
        sig (case alg
              :RS256 (rsa-sign private-key data alg-key)
              :HS256 (hmac-sign private-key data alg-key))]
    (str data "." sig)))

(def generate sign)

(defn parse [jwt-str]
  (let [parts (str/split jwt-str #"\.")]
    (when (= 3 (count parts))
      (let [[header claims signature] parts]
        {:header  (json/parse-string (String. (safe-decode header)) keyword)
         :claims  (json/parse-string (String. (safe-decode claims)) keyword)
         :body    (str header "." claims)
         :signature signature}))))

(defn rsa-verify
  "Function to verify data and signature with RSA algorithm."
  [alg public-key body signature]
  (let [sig (doto (java.security.Signature/getInstance alg)
              (.initVerify public-key)
              (.update (.getBytes body)))]
    (.verify sig (safe-decode signature))))

(defn rsa-verify-nobase64
  "Function to verify data and signature with RSA algorithm."
  [alg public-key body signature]
  (let [sig (doto (java.security.Signature/getInstance alg)
              (.initVerify public-key)
              (.update (.getBytes body)))]
    (.verify sig signature)))

(defn hmac-verify
  "Function to verify data and signature with HMAC algorithm."
  [alg key body signature]
  (crypto-eq? signature (hmac-sign key body alg)))

(defn verify [jwt public-key]
  (let [alg (keyword (get-in jwt [:header :alg]))]
    (if-let [alg-name (get algs alg)]
      (case alg
        :RS256 (rsa-verify alg-name public-key (:body jwt) (:signature jwt))
        :HS256 (hmac-verify alg-name public-key (:body jwt) (:signature jwt)))
      (throw (Exception. (str "Unknown alg: " (pr-str (:header jwt))))))))

(defn- kp-generator [length]
  (doto (java.security.KeyPairGenerator/getInstance "RSA")
    (.initialize length)))

(defn- new-keypair [length]
  (assert (>= length 512) "RSA Key must be at least 512 bits long.")
  (.generateKeyPair (kp-generator length)))

(defn- format-pem-string [encoded key-type]
  "Takes a Base64-encoded string of key data and formats it
   for file-output following openssl's convention of wrapping lines
   at 64 characters and appending the appropriate header and footer for
   the specified key type"
  (let [chunked (->> encoded
                     (partition 64 64 [])
                     (map #(apply str %)))
        formatted (str/join "\n" chunked)]
    (str "-----BEGIN " key-type "-----\n"
         formatted
         "\n-----END " key-type "-----\n")))

(defn- private-key->pem-string [key]
  "Convert RSA private keypair to a formatted PEM string for saving in
   a .pem file. By default these private keys will encode themselves as PKCS#8
   data (e.g. when calling (.getEncoded private-key)), so we have to convert it
   to ASN1, which PEM uses (this seems to also be referred to as PKCS#1).
   More info here http://stackoverflow.com/questions/7611383/generating-rsa-keys-in-pkcs1-format-in-java"
  (-> (.getEncoded ^java.security.PrivateKey key)
      (org.bouncycastle.asn1.pkcs.PrivateKeyInfo/getInstance)
      (.parsePrivateKey)
      (.toASN1Primitive)
      (.getEncoded)
      (encode64)
      (format-pem-string "RSA PRIVATE KEY")))

(defn- public-key->pem-string [key]
  "Generate PEM-formatted string for a public key. This is simply a base64
   encoding of the key wrapped with the appropriate header and footer."
  (format-pem-string (encode64 (.getEncoded ^java.security.PublicKey key)) "PUBLIC KEY"))

(defn generate-keypair [& [{:keys [rsa-key-length]
                            :or {rsa-key-length 1024}}]]
  ;; length based on https://connect2id.com/products/nimbus-jose-jwt/examples/jwt-with-rsa-signature
  ;; we double defaults because of microsoft
  (let [keypair (new-keypair rsa-key-length)
        private (.getPrivate keypair)
        public  (.getPublic keypair)]
    {:private (private-key->pem-string private)
     :public  (public-key->pem-string  public)}))


(defn uuid [] (str (java.util.UUID/randomUUID)))

(defn rand-str []
  (encode64 (uuid)))

(defn gen-hs256-secret []
  (subs (str (rand-str) (rand-str)) 0 64))

;; (hmac-sign "mysecret" "{\"user\":\"ivan\"}" "HmacSHA256")
;; (hmac-verify
;;  "HmacSHA256"
;;  "mysecret"
;;  "{\"user\":\"ivan\"}"
;;  (hmac-sign "mysecret" "{\"user\":\"ivan\"}" "HmacSHA256"))
