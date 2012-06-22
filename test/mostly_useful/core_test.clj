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
 "about rearranging the arity of a function"
 (let [flippy-vector (base/flop vector [2 0 1])
       flippy-conj (base/flop conj [3 2 1 0])]
   (flippy-vector :a :b :c) => [:c :a :b]
   (flippy-conj 1 2 3 [1 2]) => [1 2 3 2 1]))

(fact
 (base/reduce-keepv conj [] {:a 1 :b 2 :c nil}) => [:a 1 :b 2])

(fact
 (base/assoc-keep {:a 1} :c nil :d 3 :f 4 :g 9 :h false)
 => {:a 1 :d 3 :f 4 :g 9 :h false})

(facts
 "about updating keys"
 (let [m {:make "csi" :uuid "abc-123" :serial-number "40657" :model "CR1000"}]
   (base/update-keys name m)
   => {"make" "csi" "uuid" "abc-123" "serial-number" "40657" "model" "CR1000"}
   (base/update-keys keyword (base/update-keys name m)) => m))

(facts
 "about updating values"
 (let [m {:make "csi" :uuid "abc-123" :serial-number 40657 :model 1000}]
   (base/update-values str m)
   => {:make "csi" :uuid "abc-123" :serial-number "40657" :model "1000"}))
