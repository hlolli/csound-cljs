(ns csound.core
  (:require [cljs.spec.alpha :as s]
            [cljs.spec.test.alpha :as stest]
            [cljs.analyzer :refer [parse] :as anal]
            [clojure.zip :as zip]
            [clojure.string :as string])
  (:require-macros
   [csound.macros :refer [definstr]]
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
(def ^:dynamic *global* true)



(declare tree-to-assembler)


(deftype AudioSignal [ast]
  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found] (get (.-ast coll) k not-found))
  IFn
  (-invoke [coll] (tree-to-assembler (.-ast coll)))
  (-invoke [coll assembler] (into (tree-to-assembler (.-ast coll)) assembler)))

(deftype ControlSignal [ast]
  ILookup
  (-lookup [coll k not-found] (get (.-ast coll) k not-found))
  (-lookup [coll k] (get (.-ast coll) k))
  IFn
  (-invoke [coll] (tree-to-assembler (.-ast coll)))
  (-invoke [coll assembler] (into (tree-to-assembler (.-ast coll)) assembler)))

(deftype FrequencySignal [ast]
  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found] (get (.-ast coll) k not-found))
  IFn
  (-invoke [coll] (tree-to-assembler (.-ast coll)))
  (-invoke [coll assembler] (into (tree-to-assembler (.-ast coll)) assembler)))

(deftype Variable [ast]
  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found] (get (.-ast coll) k not-found))
  IFn
  (-invoke [coll] (tree-to-assembler (.-ast coll)))
  (-invoke [coll assembler] (into (tree-to-assembler (.-ast coll)) assembler)))

(deftype String [ast]
  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found] (get (.-ast coll) k not-found))
  IFn
  (-invoke [coll] (tree-to-assembler (.-ast coll)))
  (-invoke [coll assembler] (into (tree-to-assembler (.-ast coll)) assembler)))

(deftype AudioArray [ast]
  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found] (get (.-ast coll) k not-found))
  IFn
  (-invoke [coll] (tree-to-assembler (.-ast coll)))
  (-invoke [coll assembler] (into (tree-to-assembler (.-ast coll)) assembler)))

(deftype ControlArray [ast]
  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found] (get (.-ast coll) k not-found))
  IFn
  (-invoke [coll] (tree-to-assembler (.-ast coll)))
  (-invoke [coll assembler] (into (tree-to-assembler (.-ast coll)) assembler)))

(deftype VariableArray [ast]
  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found] (get (.-ast coll) k not-found))
  IFn
  (-invoke [coll] (tree-to-assembler (.-ast coll)))
  (-invoke [coll assembler] (into (tree-to-assembler (.-ast coll)) assembler)))

(deftype StringArray [ast]
  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found] (get (.-ast coll) k not-found))
  IFn
  (-invoke [coll] (tree-to-assembler (.-ast coll)))
  (-invoke [coll assembler] (into (tree-to-assembler (.-ast coll)) assembler)))

(deftype IO [ast]
  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found] (get (.-ast coll) k not-found))
  IFn
  (-invoke [coll] (tree-to-assembler (.-ast coll)))
  (-invoke [coll assembler] (into (tree-to-assembler (.-ast coll)) assembler)))

(deftype ScoreParameter [ast]
  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found] (get (.-ast coll) k not-found))
  IFn
  (-invoke [coll] (tree-to-assembler (.-ast coll)))
  (-invoke [coll assembler] (into (tree-to-assembler (.-ast coll)) assembler)))


(defn isa-csound-type? [obj]
  (#{AudioSignal ControlSignal FrequencySignal
     Variable String AudioArray ControlArray
     VariableArray StringArray IO
     ScoreParameter} (type obj)))

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

(defn- ast-node [ret-type opcode in global? & [operator]]
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
             :global? global?
             :operator operator}]
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
              out (if (coll? out) (apply str (interpose ", " out)) out)
              operator (if-let [operator (:operator node)] (str " " operator " ") ", ")]
          (conj acc [(:ident parent-node)
                     (apply str (interpose " "
                                           (cond->> [(:opcode parent-node)
                                                     (->> (map #(or (:out %) %)
                                                               (remove nil? (:in node)))
                                                          (interpose operator)
                                                          (apply str))]
                                             out (into [out]))))])))
      (reduce [] tree)))


(defn tree-to-assembler [tree]
  (-> tree
      flatten-tree
      parse-tree))

(defn parse-to-string [assembler]
  ;; Transducer here?
  (letfn [(dedupe [v]
            (reduce (fn [acc node]
                      (if (some #(= (first node) (first %)) acc)
                        acc
                        (conj acc node))) [] v))
          (reduce-to-string [v]
            (reduce (fn [acc-str [_ s]]
                      (str acc-str s "\n")) "" v))]
    (-> assembler dedupe reduce-to-string)))

(defn cljs->csound-instr-name [instr-name]
  (-> instr-name
      (string/replace #"-" "_")))

(defn score-parameter? [v]
  (= ScoreParameter (type v)))

(defn valid-ar? [v]
  (= AudioSignal (type v)))

(defn valid-ar?* [v]
  (or (valid-ar? v)
      (nil? v)))

(defn valid-kr? [v]
  (or (= ControlSignal (type v))
      (= ScoreParameter (type v))
      (= Variable (type v))
      (number? v)))

(defn valid-kr?* [v]
  (or (valid-kr? v)
      (nil? v)))

(defn valid-i? [v]
  (or (= ScoreParameter (type v))
      (= Variable (type v))
      (number? v)))

(defn valid-i?* [v]
  (or (valid-i? v)
      (nil? v)))

(defn valid-f? [v]
  (= FrequencySignal (type v)))

(defn valid-f?* [v]
  (or (valid-f? v)
      (nil? v)))

(defn valid-S? [v]
  (or (= ScoreParameter (type v))
      (= String (type v))
      (number? v)))

(defn valid-S?* [v]
  (or (valid-S? v)
      (nil? v)))

(defn valid-SArr? [v]
  (= StringArray (type v)))

(defn valid-SArr?* [v]
  (or (valid-SArr? v)
      (nil? v)))

(defn valid-aArr? [v]
  (= AudioArray (type v)))

(defn valid-aArr?* [v]
  (or valid-aArr?
      (nil? v)))

(defn valid-kArr? [v]
  (= ControlArray (type v)))

(defn valid-kArr?* [v]
  (or valid-kArr?
      (nil? v)))

(defn valid-iArr? [v]
  (= VariableArray (type v)))

(defn valid-iArr?* [v]
  (or valid-iArr?
      (nil? v)))

(defn valid-x? [v]
  (or (valid-ar? v)
      (valid-i? v)
      (valid-kr? v)
      (score-parameter? v)
      (valid-S? v)
      (valid-aArr? v)
      (valid-kArr? v)
      (valid-iArr? v)))

(defn valid-x?* [v]
  (or valid-x?
      (nil? v)))



;; (println (parse-to-string ((comp asig3 (out asig3)))))


;; (load-file "src/csound/opcodes.cljs")


(comment
  (defn poscil [amp cps & [ifn]]
    (let [_ (prn "GLOBAL: " *global*)
          out-types-quoted 'AudioSignal
          out-types AudioSignal
          ast (ast-node out-types-quoted
                        "poscil"
                        [amp cps ifn]
                        false)]
      (new out-types ast)))

  (s/fdef poscil:a
    :args (s/alt
           :aaii (s/cat :amp valid-ar? :cps valid-ar? :table* valid-i?* :phase* valid-i?* )
           ))
  
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

  ;; (def asig1 (poscil2 1 100 1))
  ;; (def asig10 (poscil 1 100 1))
  ;; (def asig2 (poscil2 asig1 100 1))
  ;; (def asig3 (poscil (last asig2) 6 0))


  (definstr abc []
    (let [_ (prn *global*)
          sig (poscil 1 100 1)]
      sig))

  (println (abc))

  )
