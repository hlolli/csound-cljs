(ns csound.operators
  (:require csound.core))

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
               "/")]
      (new out-type ast))
    (apply cljs.core/* vals)))

(def p3 (csound.core/ScoreParameter. 3))

