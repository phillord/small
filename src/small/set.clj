(ns small.set
  (:use [backtick]))

(declare set0)
(declare set0fn)
(declare create-set1)
(declare create-set2)
(declare create-set3)
(declare create-set4)

(defn disjoin-form [one-smaller
                    member-symbols
                    key-symbol
                    this-symbol]
  `(cond
    ~@(apply concat
             (map
              (fn [x]
                (list
                 (list `= x key-symbol)
                 (list* one-smaller
                        (remove #{x} member-symbols))))
              member-symbols))
    :else ~this-symbol))


(defmacro defset [name
                  params
                  set-1
                  set+1]
  (template
   (deftype ~name ~params
     clojure.lang.IPersistentSet
     (cons [this v]
       (if (.contains this v)
         this
         (~set+1 ~@params v)))
     (empty [this]
       small.set/set0)
     (equiv [this that]
       (.equals this that))
     (seq [this]
       (list ~@params))
     (get [this n]
       (clojure.core/when (.contains this n)
         n))
     (contains [this n]
       (clojure.core/true?
        (clojure.core/or
         ~@(clojure.core/map
            (fn [param]
              `(= ~'n ~param))
            params))))
     (disjoin [this key]
       ~(disjoin-form
          set-1
          params
          'key
          'this))
     clojure.lang.IFn
     (invoke [this n]
       (.contains this n))
     Object
     (toString [this] (pr-str this)))))

(defset Set0 []
  ;; could be anything, since we always return this..
  identity
  create-set1)

(def
  set0 (Set0.))

(defn set0fn []
  set0)

(defset Set1 [one]
  set0fn
  create-set2)

(defn create-set1 [one]
  (Set1. one))

(defset Set2 [one two]
  create-set1
  create-set3)

(defn create-set2 [one two]
  (Set2. one two))

;; the last hash-set call is very slow because we must construct the hashset
;; from first principles. So, what we really need to do is to tail share with
;; the previous set, which would be a lot quicker.
;;
;; Currently, this is a pain because the cons method doesn't pass this through
;; which we need. So, we need to pass this through each of the create-set
;; methods which we can ignore except for the last.

(defset Set3 [one two three]
  create-set2
  create-set4)

(defn create-set3 [one two three]
  (Set3. one two three))

(defset Set4 [one two three four]
   create-set3
   hash-set)

(defn create-set4 [one two three four]
  (Set4. one two three four))


(defn small-set
  ([]
     set0)
  ([one]
     (Set1. one))
  ([one two]
     (Set2. one two))
  ([one two three]
     (Set3. one two three))
  ([one two three four]
     (Set4. one two three four)))
