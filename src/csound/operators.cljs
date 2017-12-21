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


(defn if:i [& [comparator then else :as form]]
  (when (< (count form) 3)
    (throw (js/Error. "Too few arguments to if:i")))
  (when (> (count form) 4)
    (throw (js/Error. "Too many arguments to if:i")))
  (assert (= (type comparator) csound.core/CsoundComparator)
          "Error, test case for if:i is not a Boolean (Csound Comparator)")
  (when (and then else)
    (if (seqable? then)
      (assert (= (map type then) (map type else))
              "Type mismatch in if:i statement, then and else clauses must return same rate")
      (assert (= (type then) (type else))
              "Type mismatch in if:i statement, then and else clauses must return same rate")))
  (let [ast (csound.core/ast-node
             'CsoundBoolean
             "igoto"
             [comparator then else]
             csound.core/*global*)
        then (if (seqable? then)
               (mapv #(csound.core/-attachBoolean % ast 0) then)
               (csound.core/-attachBoolean then ast 0))
        ;; else (if (seqable? then)
        ;;        (mapv #(csound.core/-attachBoolean % ast 1) else)
        ;;        (csound.core/-attachBoolean else ast 1))
        ]
    
    ;; Attention, just return the `then` case, given all
    ;; cases are of same rate, they will return the same ident
    ;; parseing logic takes place elsewhere
    then))

(comment 
  (if:i (= (csound.opcodes/init:i 1) (csound.opcodes/init:i 1))
        (csound.core/Variable. {})
        (csound.core/Variable. {}))
  )
