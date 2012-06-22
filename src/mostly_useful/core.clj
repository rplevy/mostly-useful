(ns mostly-useful.core)

(defmacro if-pred-let
  "If test is true, evaluates then with binding-form bound to the value of
  test, if not, yields else"
  ([pred bindings then]
   `(if-let ~bindings ~then nil))
  ([pred bindings then else]
     (let [form (bindings 0) tst (bindings 1)]
     `(let [temp# ~tst]
        (if (~pred temp#)
          (let [~form temp#]
            ~then)
          ~else)))))

(defmacro when-pred-let
  "When test is true, evaluates body with binding-form bound to value of test"
  [pred bindings & body]
  (let [form (bindings 0) tst (bindings 1)]
    `(let [temp# ~tst]
       (when (~pred temp#)
         (let [~form temp#]
           ~@body)))))

(defn flip [f] (fn [a b] (f b a)))

(defn reduce-keepv
  "reduce-kv with rejection of nil values"
  [f init coll]
  (reduce-kv (fn [r k v] (if-not (nil? v) (f r k v) r)) init coll))

(defn assoc-keep
  "only assoc non-null values"
  [m & {:as kv}]
  (reduce-keepv assoc m kv))

(defn update-keys [f m]
  (reduce-kv #(assoc %1 (f %2) %3) {} m))
