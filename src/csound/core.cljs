(ns csound.core
  (:require [cljs.spec.alpha :as s]
            [cljs.spec.test.alpha :as stest]
            [cljs.analyzer :refer [parse] :as anal]
            [clojure.zip :as zip]
            [csound.metadata :refer [metadata-db]])
  (:require-macros
   [cljs.analyzer.macros
    :refer [disallowing-recur allowing-redef]]))



#_(defmethod parse 'if
    [op env [_ test then else :as form] name _]
    (when (< (count form) 3)
      (throw (anal/error env "Too few arguments to if")))
    (when (> (count form) 4)
      (throw (anal/error env "Too many arguments to if")))
    (let [test-expr (disallowing-recur (anal/analyze (assoc env :context :expr) test))
          then-expr (allowing-redef (anal/analyze env then))
          else-expr (allowing-redef (anal/analyze env else))]
      {:env env :op :if :form form
       :test test-expr :then then-expr :else else-expr
       :unchecked *unchecked-if*
       :children [test-expr then-expr else-expr]}))


(def ^:dynamic *0dbfs* 1)
(def ^:dynamic *nchnls* 2)
(def ^:dynamic *sr* 44100)
(def ^:dynamic *ksmps* 16)
(def ^:dynamic *A4* 440)



(declare assemble-tree)


(deftype IO [ast]
  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found] (get (.-ast coll) k not-found))
  IFn
  (-invoke [coll] (assemble-tree (.-ast coll)))
  (-invoke [coll assembler] (into (assemble-tree (.-ast coll)) assembler)))

(deftype AudioSignal [ast]
  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found] (get (.-ast coll) k not-found))
  IFn
  (-invoke [coll] (assemble-tree (.-ast coll)))
  (-invoke [coll assembler] (into (assemble-tree (.-ast coll)) assembler)))

(deftype ControlSignal [ast]
  ILookup
  (-lookup [coll k not-found] (get (.-ast coll) k not-found))
  (-lookup [coll k] (get (.-ast coll) k))
  IFn
  (-invoke [coll] (assemble-tree (.-ast coll)))
  (-invoke [coll assembler] (into (assemble-tree (.-ast coll)) assembler)))

(deftype FrequencySignal [ast]
  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found] (get (.-ast coll) k not-found))
  IFn
  (-invoke [coll] (assemble-tree (.-ast coll)))
  (-invoke [coll assembler] (into (assemble-tree (.-ast coll)) assembler)))

(deftype Variable [ast]
  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found] (get (.-ast coll) k not-found))
  IFn
  (-invoke [coll] (assemble-tree (.-ast coll)))
  (-invoke [coll assembler] (into (assemble-tree (.-ast coll)) assembler)))

(deftype String [ast]
  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found] (get (.-ast coll) k not-found))
  IFn
  (-invoke [coll] (assemble-tree (.-ast coll)))
  (-invoke [coll assembler] (into (assemble-tree (.-ast coll)) assembler)))

(deftype AudioArray [ast]
  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found] (get (.-ast coll) k not-found))
  IFn
  (-invoke [coll] (assemble-tree (.-ast coll)))
  (-invoke [coll assembler] (into (assemble-tree (.-ast coll)) assembler)))

(deftype ControlArray [ast]
  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found] (get (.-ast coll) k not-found))
  IFn
  (-invoke [coll] (assemble-tree (.-ast coll)))
  (-invoke [coll assembler] (into (assemble-tree (.-ast coll)) assembler)))

(deftype VariableArray [ast]
  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found] (get (.-ast coll) k not-found))
  IFn
  (-invoke [coll] (assemble-tree (.-ast coll)))
  (-invoke [coll assembler] (into (assemble-tree (.-ast coll)) assembler)))

(deftype StringArray [ast]
  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found] (get (.-ast coll) k not-found))
  IFn
  (-invoke [coll] (assemble-tree (.-ast coll)))
  (-invoke [coll assembler] (into (assemble-tree (.-ast coll)) assembler)))


(defn isa-csound-type? [obj]
  (#{AudioSignal ControlSignal FrequencySignal
     Variable String AudioArray ControlArray
     VariableArray StringArray} (type obj)))

(defn- gen-symbol [ret-type global?]
  (str
   (when global? "g")
   (case ret-type
     AudioSignal (-> "a" gensym str)
     ControlSignal (-> "k" gensym str)
     FrequencySignal (-> "f" gensym str)
     Variable (-> "i" gensym str)
     String (-> "S" gensym str)
     AudioArray (str "a" (gensym "0") "[]")
     ControlArray (str "k" (gensym "0") "[]")
     VariableArray (str "i" (gensym "0") "[]")
     StringArray (str "S" (gensym "0") "[]")
     )))

(defn- ast-node [ret-type opcode in global?]
  (let [child (fn [ident parent-ast in out]
                {:ident ident
                 :parent parent-ast
                 :out out
                 :in in})
        out (if (= ret-type 'IO)
              nil
              (if (seqable? ret-type)
                (mapv #(gen-symbol %1 global?) ret-type)
                (gen-symbol ret-type global?)))
        ast {:ident (str (gensym))
             :opcode  opcode
             :in      in
             :out     out
             :global? global?}]
    (if (seqable? ret-type)
      (mapv #(child (:ident ast) ast (:in ast) %) out)
      ast)))


(defn dedupe-nodes [v children]
  (let [cidents (map first children)]
    (reduce (fn [acc node]
              (if-not (some #(= (first node) %) cidents)
                (conj acc node)
                acc)) [] v)))

(defn flatten-tree [root-node]
  (loop [v [[(:ident root-node) root-node]]
         q (filter isa-csound-type? (:in root-node))]
    (if (empty? q)
      (reverse v)
      (let [children (mapv #(vector (:ident %) %) q)
            v (dedupe-nodes v children)
            v (into v children)
            q (flatten (map #(filter isa-csound-type? (:in (second %))) children))]
        (recur v q)))))

(defn parse-tree [tree]
  (-> (fn [acc node]
        (let [[ident node] node
              parent-node (if (contains? node :parent) (:parent node) node)
              out (:out parent-node)
              out (if (coll? out) (apply str (interpose ", " out)) out)]
          (conj acc [(:ident parent-node)
                     (apply str (interpose " "
                                           (cond->> [(:opcode parent-node)
                                                     (->> (map #(or (:out %) %) (:in node))
                                                          (interpose ", ")
                                                          (apply str))]
                                             out (into [out]))))])))
      (reduce [] tree)))

(defn assemble-tree [csnd]
  (-> csnd
      flatten-tree
      parse-tree))



(defn poscil [& [amp cps ifn]]
  (let [out-types-quoted 'AudioSignal
        out-types AudioSignal
        ast (ast-node out-types-quoted
                      "poscil"
                      [amp cps ifn]
                      false)]
    (new out-types ast)))

(defn poscil2 [& [amp cps ifn]]
  (let [out-types-quoted '[AudioSignal AudioSignal]
        out-types [AudioSignal AudioSignal]
        ast (ast-node out-types-quoted
                      "poscil"
                      [amp cps ifn]
                      false)]
    (mapv #(new %1 %2) out-types ast)))

(defn out [signal]
  (let [out-types-quoted 'IO
        out-types IO
        ast (ast-node out-types-quoted
                      "out"
                      [signal]
                      false)]
    (new out-types ast)))


(def asig1 (poscil 1 100 1))
(def asig10 (poscil 1 100 1))
(def asig2 (poscil2 asig1 100 1))
(def asig3 (poscil (last asig2) 6 0))

(defn parse-to-string [csnd]
  ;; Transducer here?
  (letfn [(dedupe [v]
            (reduce (fn [acc node]
                      (if (some #(= (first node) (first %)) acc)
                        acc
                        (conj acc node))) [] v))
          (reduce-to-string [v]
            (reduce (fn [acc-str [_ s]]
                      (str acc-str s "\n")) "" v))]
    (-> csnd dedupe reduce-to-string)))

(println (parse-to-string ((comp asig3 (out asig3)))))

(ns csound.core$macros)



(csound.core$macros/definst a [] :a)


((comp asig1
       asig10) [])

(asig1 (asig10))


(comment 

  (time (doseq [_ (range 10000)] (parse-tree (flatten-tree asig3))
               ))

  (time (doseq [_ (range 10000)] (apply + (range 100))))


  (def asig1 (AudioSignal. (ast-node '[AudioSignal AudioSignal]  "poscil" [1 440 1] false false)))
  (def asig2 (AudioSignal. (ast-node 'AudioSignal  "poscil" [asig1 440 1] false false)))
  (def asig3 (AudioSignal. (ast-node 'AudioSignal  "poscil" [asig2 440 1] false false)))
  (def asig4 (AudioSignal. (ast-node 'AudioSignal  "poscil" [asig3 asig1 1] false false)))

  (defn glob []
    (meta #'glob))

  (def ^:dynamic a (glob))

  (parse-tree (flatten-tree asig4))

  (-> (zip/seq-zip (seq (xxx. [(xxx. 100) (xxx. 666)])))
      zip/next)

  (-> (zip/vector-zip [1 [2 [3]]])
      zip/next
      zip/next
      zip/next
      zip/next
      zip/next
      zip/next)

  (tree-seq sequential? seq [1 [2 [3]]])

  (def asig (AudioSignal. 100 false))
  (def ksig (AudioSignal. [:a :b :c 66]))

  (def ^:private valid-x-rates #{CsoundAudioSignal CsoundControlSignal CsoundVariable Number})

  (def ^:private valid-k-rates #{CsoundControlSignal CsoundVariable Number})

  (def ^:private valid-i-rates #{CsoundVariable Number})

  (def ^:private valid-a-rates #{CsoundAudioSignal})

  (.-a-var asig)

  (def b {:a 1 :b (:a b)})

  (defn k [number]
    (CsoundControlSignal. number))

  (map type (list ksig asig))

  (defn a-rate? [v]
    (= CsoundAudioSignal (type v)))

  (defn k-rate? [v]
    (= CsoundControlSignal (type v)))

  (defn f-rate? [v]
    (= CsoundSpectralSignal (type v)))





  (defn pattern-match-parameters [])

  (defn poscil
    ;; [& [p1 p2 p3 p4 :as env]]
    {:arglists '([p1 p2 p3 p4] [i1 i2 i3 i4])}
    [p1 p2 p3 p4]
    [p1 p2 p3 p4])

  (meta #'poscil)

  (poscil 2 :p2 2)

  (keys (methods poscil))

  (defmethod poscil
    [CsoundAudioSignal CsoundAudioSignal CsoundVariable CsoundVariable]
    [a1 a2 j1 o1]
    (let [out-type CsoundAudioSignal]
      (ast-node (gensym out-type)
                "poscil"
                [a1 a2 j1 o1]
                out-type
                false)))

  (defmethod poscil
    [CsoundControlSignal CsoundControlSignal CsoundVariable CsoundVariable]
    [a1 a2 j1 o1]
    (let [out-type CsoundAudioSignal]
      (ast-node (gensym out-type)
                "poscil"
                [a1 a2 j1 o1]
                out-type
                false)))

  (def sig1 (poscil 0.1 440))


  (defn a [& {:keys [a1 a2 a3 o1]
              :or {o1 1} :as env}]
    [env a1 o1])

  (a 1 2 3 4 :a1 1)
  (s/def :: string?)

  (stest/instrument `silly)

  (defn silly [x]
    x)

  (s/fdef silly
    :args #(s/cat :x string?)
    :ret string?
    :fn string?)

  (stest/check `silly)

  (silly [""])

  (lumo.repl/doc silly)
  )
