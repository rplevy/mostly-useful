(ns mostly-useful.core-test
  (:require [mostly-useful.core :as base]
            [midje.sweet :refer :all]))

(facts
 "when-pred-let & if-pred-let"
 (when-let [foo []] foo) => []
 (base/when-pred-let seq [foo []] foo) => nil
 (base/when-pred-let (partial < 0) [foo 1] foo) => 1
 (base/if-pred-let seq [foo []] foo 2) => 2
 (base/if-pred-let (partial < 0) [foo 1] foo 2) => 1)

(fact
 "flip"
 ((base/flip conj) 4 [1 2 3]) => [1 2 3 4])

(facts
 "prn> & prn>>"
 (with-out-str (-> 1 (base/prn> "quux") list)) => "\"quux\" 1\n"
 (with-out-str (->> 1 (base/prn>> "mary") list)) => "\"mary\" 1\n"
 (with-out-str
   (-> 1 (base/prn> "quux") list) => [1]
   (->> 1 (base/prn>> "mary") list) => [1]))

(fact
 (base/assoc-keep {:a 1} :c nil :d 3 :f 4 :g 9)
 => {:a 1 :d 3 :f 4 :g 9})

(facts "about converting keys"
  (let [m {:make "csi" :uuid "abc-123" :serial-number "40657" :model "CR1000"}]
    (base/convert-keys name m)
    => {"make" "csi" "uuid" "abc-123" "serial-number" "40657" "model" "CR1000"}
    (base/convert-keys keyword (base/convert-keys name m)) => m))
