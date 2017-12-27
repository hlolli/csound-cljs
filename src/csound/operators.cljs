(ns csound.operators
  (:require csound.core
            csound.opcodes
            [cljs.analyzer :refer [parse] :as anal])
  (:require-macros [cljs.analyzer.macros
                    :refer [disallowing-recur allowing-redef]]))

;; The only p constants, otherwise definstr argnames
(def p1 (csound.core/ScoreParameter. 1))
(def p2 (csound.core/ScoreParameter. 2))
(def p3 (csound.core/ScoreParameter. 3))

(defn resolve-fastest-rate [vals]
  (cond
    (some #(= csound.core/AudioSignal (type %)) vals)
    ['AudioSignal csound.core/AudioSignal]
    (some #(= csound.core/ControlSignal (type %)) vals)
    ['ControlSignal csound.core/ControlSignal]
    :else
    ['Variable csound.core/Variable]))

(defn mutate! [object expression]
  (csound.core/-mutateOutput expression object))

(defn += [object expression]
  (let [sum-ast (csound.core/ast-node
                 (:ret-type-quoted object)
                 "+="
                 [expression]
                 csound.core/*global*)
        obj-type (type object)]
    (csound.core/-mutateOutput (new obj-type sum-ast) object)))

(defn + [& vals]
  (if (some csound.core/isa-csound-type? vals)
    (let [[out-type-quoted out-type]
          (resolve-fastest-rate vals)
          ast (csound.core/ast-node
               out-type-quoted
               "="
               (vec vals)
               csound.core/*global*
               "+")]
      (new out-type ast))
    (apply cljs.core/+ vals)))

(defn - [& vals]
  (if (some csound.core/isa-csound-type? vals)
    (let [[out-type-quoted out-type]
          (resolve-fastest-rate vals)
          ast (csound.core/ast-node
               out-type-quoted
               "="
               (vec vals)
               csound.core/*global*
               "-")]
      (new out-type ast))
    (apply cljs.core/+ vals)))

(defn / [& vals]
  (if (some csound.core/isa-csound-type? vals)
    (let [[out-type-quoted out-type]
          (resolve-fastest-rate vals)
          ast (csound.core/ast-node
               out-type-quoted
               "="
               (vec vals)
               csound.core/*global*
               "/")]
      (new out-type ast))
    (apply cljs.core// vals)))

(defn * [& vals]
  (if (some csound.core/isa-csound-type? vals)
    (let [[out-type-quoted out-type]
          (resolve-fastest-rate vals)
          ast (csound.core/ast-node
               out-type-quoted
               "="
               (vec vals)
               csound.core/*global*
               "*")]
      (new out-type ast))
    (apply cljs.core/* vals)))

(defn min [& vals]
  (if (some csound.core/isa-csound-type? vals)
    (let [[out-type-quoted out-type]
          (resolve-fastest-rate vals)
          ast (csound.core/ast-node
               out-type-quoted
               "="
               (vec vals)
               csound.core/*global*
               "*")]
      (new out-type ast))
    (apply cljs.core/* vals)))

(defn % [& vals]
  (if (some csound.core/isa-csound-type? vals)
    (let [[out-type-quoted out-type]
          (resolve-fastest-rate vals)
          ast (csound.core/ast-node
               out-type-quoted
               "="
               (vec vals)
               csound.core/*global*
               "%")]
      (new out-type ast))
    (throw (js/Error. "% is not a Clojure function, use `mod` or `rem` instead"))))

(defn = [& vals]
  (if (some csound.core/isa-csound-type? vals)
    (do
      (assert (cljs.core/= 2 (count vals)) "Csound compareators must have exacly two arguments")
      (let [[out-type-quoted out-type] (resolve-fastest-rate vals)
            ast (csound.core/ast-node
                 'CsoundComparator
                 nil
                 (vec vals)
                 csound.core/*global*
                 "==")]
        (new csound.core/CsoundComparator ast)))
    (apply cljs.core/= vals)))

(defn < [& vals]
  (if (some csound.core/isa-csound-type? vals)
    (do
      (assert (cljs.core/= 2 (count vals)) "Csound compareators must have exacly two arguments")
      (let [[out-type-quoted out-type] (resolve-fastest-rate vals)
            ast (csound.core/ast-node
                 'CsoundComparator
                 nil
                 (vec vals)
                 csound.core/*global*
                 "<")]
        (new csound.core/CsoundComparator ast)))
    (apply cljs.core/< vals)))

(defn > [& vals]
  (if (some csound.core/isa-csound-type? vals)
    (do
      (assert (cljs.core/= 2 (count vals)) "Csound compareators must have exacly two arguments")
      (let [[out-type-quoted out-type] (resolve-fastest-rate vals)
            ast (csound.core/ast-node
                 'CsoundComparator
                 nil
                 (vec vals)
                 csound.core/*global*
                 ">")]
        (new csound.core/CsoundComparator ast)))
    (apply cljs.core/> vals)))

(defn <= [& vals]
  (if (some csound.core/isa-csound-type? vals)
    (do
      (assert (cljs.core/= 2 (count vals)) "Csound compareators must have exacly two arguments")
      (let [[out-type-quoted out-type] (resolve-fastest-rate vals)
            ast (csound.core/ast-node
                 'CsoundComparator
                 nil
                 (vec vals)
                 csound.core/*global*
                 "<=")]
        (new csound.core/CsoundComparator ast)))
    (apply cljs.core/<= vals)))

(defn >= [& vals]
  (if (some csound.core/isa-csound-type? vals)
    (do
      (assert (cljs.core/= 2 (count vals)) "Csound compareators must have exacly two arguments")
      (let [[out-type-quoted out-type] (resolve-fastest-rate vals)
            ast (csound.core/ast-node
                 'CsoundComparator
                 nil
                 (vec vals)
                 csound.core/*global*
                 ">=")]
        (new csound.core/CsoundComparator ast)))
    (apply cljs.core/>= vals)))


(defn != [& vals]
  (if (some csound.core/isa-csound-type? vals)
    (do
      (assert (cljs.core/= 2 (count vals)) "Csound compareators must have exacly two arguments")
      (let [[out-type-quoted out-type] (resolve-fastest-rate vals)
            ast (csound.core/ast-node
                 'CsoundComparator
                 nil
                 (vec vals)
                 csound.core/*global*
                 "!=")]
        (new csound.core/CsoundComparator ast)))
    (throw (js/Error. "!= is not a Clojure function"))))

(def tied?
  (let [ast (csound.core/ast-node
             'CsoundComparator
             nil
             nil
             csound.core/*global*
             "tigoto")]
    (new csound.core/CsoundComparator ast)))

(defn- number-to-variable [val]
  (if (seqable? val)
    (reduce (fn [i v]
              (conj i (if (number? v)
                        (new csound.core/Variable
                             (csound.core/ast-node
                              'Variable
                              "="
                              [v]
                              csound.core/*global*))
                        v))) [] val)
    (if (number? val)
      (new csound.core/Variable
           (csound.core/ast-node
            'Variable
            "="
            [val]
            csound.core/*global*))
      val)))

(defn if:i [& [comparator then else :as form]]
  (when (< (count form) 3)
    (throw (js/Error. "Too few arguments to if:i")))
  (when (> (count form) 4)
    (throw (js/Error. "Too many arguments to if:i")))
  (assert (= (type comparator) csound.core/CsoundComparator)
          "Error, test case for if:i is not a Boolean (Csound Comparator)")
  (when (and then else)
    (if (seqable? then)
      (do #_(assert (= (map type then) (map type else))
                    "Type mismatch in if:i statement, then and else clauses must return same rates")
          (assert (not (some #(or (= csound.core/AudioSignal (type %))
                                  (= csound.core/ControlSignal (type %))) then))
                  "if:i does not work for Audio rates and Control rates, use if:k instead"))
      (do #_(assert (= (type then) (type else))
                    "Type mismatch in if:i statement, then and else clauses must return same rate")
          (assert (not (or (= csound.core/AudioSignal (type then))
                           (= csound.core/ControlSignal (type then))))
                  "if:i does not work for Audio rates and Control rates, use if:k instead"))))
  (let [[then else] [(number-to-variable then)
                     (number-to-variable else)]
        new-out (if (seqable? then)
                  (mapv #(csound.core/regenerate-out-symbols %) then)
                  (csound.core/regenerate-out-symbols then))
        attach-new-out (fn [obj]
                         (if (seqable? then)
                           (mapv #(csound.core/-attachNewOut %1 %2) obj new-out)
                           (csound.core/-attachNewOut obj new-out)))
        ast (csound.core/ast-node
             'CsoundBoolean
             "igoto"
             [comparator
              (attach-new-out then)
              (attach-new-out else)]
             csound.core/*global*)
        then (if (seqable? then)
               (mapv #(csound.core/-attachBoolean % ast new-out) then)
               (csound.core/-attachBoolean then ast new-out))]    
    ;; Attention, just return the `then` case, given all
    ;; cases are of same rate, they will return the same ident
    ;; parseing logic takes place elsewhere
    then))

(defn if:k [& [comparator then else :as form]]
  (when (< (count form) 2)
    (throw (js/Error. "Too few arguments to if:k")))
  (when (> (count form) 4)
    (throw (js/Error. "Too many arguments to if:k")))
  (assert (= (type comparator) csound.core/CsoundComparator)
          "Error, test case for if:k is not a Boolean (Csound Comparator)")
  #_(when (and then else)
      (if (seqable? then)
        (assert (= (map type then) (map type else))
                "Type mismatch in if:k statement, then and else clauses must return same rates")
        (assert (= (type then) (type else))
                "Type mismatch in if:k statement, then and else clauses must return same rate")))
  (let [[then else] [(number-to-variable then)
                     (number-to-variable else)]
        new-out (if (seqable? then)
                  (mapv #(csound.core/regenerate-out-symbols %) then)
                  (csound.core/regenerate-out-symbols then))
        attach-new-out (fn [obj]
                         (if (seqable? then)
                           (mapv #(csound.core/-attachNewOut %1 %2) obj new-out)
                           (csound.core/-attachNewOut obj new-out)))
        ast (csound.core/ast-node
             'CsoundBoolean
             "kgoto"
             [comparator
              (attach-new-out then)
              (attach-new-out else)]
             csound.core/*global*)
        then (if (seqable? then)
               (mapv #(csound.core/-attachBoolean % ast new-out) then)
               (csound.core/-attachBoolean then ast new-out))]
    ;; Attention, just return the `then` case, given all
    ;; cases are of same rate, they will return the same ident
    ;; parseing logic takes place elsewhere
    then))



