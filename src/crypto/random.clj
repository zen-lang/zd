(ns crypto.random
  "Drop-in replacement for crypto-random 1.2.1.
   Avoids transitive dep on Clojure 1.2.1 in POM (CVE-2017-20189).
   Only fn used by ring-core: bytes."
  (:refer-clojure :exclude [bytes])
  (:import [java.security SecureRandom]))

(defn bytes
  "Returns a random byte array of the specified size."
  ^bytes [size]
  (let [seed (byte-array size)]
    (.nextBytes (SecureRandom.) seed)
    seed))
