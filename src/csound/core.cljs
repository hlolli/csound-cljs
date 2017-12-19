(ns csound.core
  (:require [cljs.spec.alpha :as s]
            [cljs.spec.test.alpha :as stest]
            [cljs.analyzer :refer [parse] :as anal]
            [clojure.zip :as zip]
            [clojure.string :as string]
            [csound.connection :as conn])
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
(def ^:dynamic *global* nil)



(declare tree-to-assembler
         global-tree-to-out)


(deftype AudioSignal [ast]
  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found] (get (.-ast coll) k not-found))
  IFn
  (-invoke [coll] (tree-to-assembler (.-ast coll)))
  (-invoke [coll assembler] (into (tree-to-assembler (.-ast coll)) assembler))
  IEmptyableCollection
  (-empty [coll] (new AudioSignal (dissoc (.-ast coll) :patch))))

(deftype ControlSignal [ast]
  ILookup
  (-lookup [coll k not-found] (get (.-ast coll) k not-found))
  (-lookup [coll k] (get (.-ast coll) k))
  IFn
  (-invoke [coll] (tree-to-assembler (.-ast coll)))
  (-invoke [coll assembler] (into (tree-to-assembler (.-ast coll)) assembler))
  IEmptyableCollection
  (-empty [coll] (new ControlSignal (dissoc (.-ast coll) :patch))))

(deftype FrequencySignal [ast]
  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found] (get (.-ast coll) k not-found))
  IFn
  (-invoke [coll] (tree-to-assembler (.-ast coll)))
  (-invoke [coll assembler] (into (tree-to-assembler (.-ast coll)) assembler))
  IEmptyableCollection
  (-empty [coll] (new FrequencySignal (dissoc (.-ast coll) :patch))))

(deftype Variable [ast]
  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found] (get (.-ast coll) k not-found))
  IFn
  (-invoke [coll] (tree-to-assembler (.-ast coll)))
  (-invoke [coll assembler] (into (tree-to-assembler (.-ast coll)) assembler))
  IEmptyableCollection
  (-empty [coll] (new Variable (dissoc (.-ast coll) :patch))))

(deftype String [ast]
  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found] (get (.-ast coll) k not-found))
  IFn
  (-invoke [coll] (tree-to-assembler (.-ast coll)))
  (-invoke [coll assembler] (into (tree-to-assembler (.-ast coll)) assembler))
  IEmptyableCollection
  (-empty [coll] (new String (dissoc (.-ast coll) :patch))))

(deftype AudioArray [ast]
  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found] (get (.-ast coll) k not-found))
  IFn
  (-invoke [coll] (tree-to-assembler (.-ast coll)))
  (-invoke [coll assembler] (into (tree-to-assembler (.-ast coll)) assembler))
  IEmptyableCollection
  (-empty [coll] (new AudioArray (dissoc (.-ast coll) :patch))))

(deftype ControlArray [ast]
  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found] (get (.-ast coll) k not-found))
  IFn
  (-invoke [coll] (tree-to-assembler (.-ast coll)))
  (-invoke [coll assembler] (into (tree-to-assembler (.-ast coll)) assembler))
  IEmptyableCollection
  (-empty [coll] (new ControlArray (dissoc (.-ast coll) :patch))))

(deftype VariableArray [ast]
  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found] (get (.-ast coll) k not-found))
  IFn
  (-invoke [coll] (tree-to-assembler (.-ast coll)))
  (-invoke [coll assembler] (into (tree-to-assembler (.-ast coll)) assembler))
  IEmptyableCollection
  (-empty [coll] (new VariableArray (dissoc (.-ast coll) :patch))))

(deftype StringArray [ast]
  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found] (get (.-ast coll) k not-found))
  IFn
  (-invoke [coll] (tree-to-assembler (.-ast coll)))
  (-invoke [coll assembler] (into (tree-to-assembler (.-ast coll)) assembler))
  IEmptyableCollection
  (-empty [coll] (new StringArray (dissoc (.-ast coll) :patch))))

(deftype IO [ast]
  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found] (get (.-ast coll) k not-found))
  IFn
  (-invoke [coll] (tree-to-assembler (.-ast coll)))
  (-invoke [coll assembler] (into (tree-to-assembler (.-ast coll)) assembler))
  IEmptyableCollection
  (-empty [coll] (new IO (dissoc (.-ast coll) :patch))))

(deftype ScoreParameter [p-num]
  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found] (str "p" (.-p-num coll)))
  IFn
  (-invoke [coll] (str "p" (.-p-num coll))))

(deftype Instrument []
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
(def
  ^{:jsdoc ["@type {*}"]}
  csnd_gensym_counter nil)

(defn- csnd-gensym
  ([] (csnd-gensym "G__"))
  ([prefix-string]
   (when (nil? csnd_gensym_counter)
     (set! csnd_gensym_counter (atom 0)))
   (str prefix-string (swap! csnd_gensym_counter inc))))

(defn- gen-symbol [ret-type global?]
  (str
   (when global? "g")
   (case ret-type
     AudioSignal (-> "a" csnd-gensym)
     ControlSignal (-> "k" csnd-gensym)
     FrequencySignal (-> "f" csnd-gensym)
     Variable (-> "i" csnd-gensym)
     String (-> "S" csnd-gensym)
     AudioArray (str "a" (csnd-gensym "0") "[]")
     ControlArray (str "k" (csnd-gensym "0") "[]")
     VariableArray (str "i" (csnd-gensym "0") "[]")
     StringArray (str "S" (csnd-gensym "0") "[]")
     )))

;; TODO: throw error on global? = nil
(defn- ast-node [ret-type opcode in global? & [operator patch]]
  (let [resolve-p (fn [i] (reduce #(if (= ScoreParameter (type %2))
                                     (conj %1 (%2))
                                     (conj %1 %2)) [] i))
        child (fn [ident parent-ast in out]
                {:ident ident
                 :parent parent-ast
                 :out out
                 :in in})
        out (if (= ret-type 'IO)
              nil
              (if (seqable? ret-type)
                (mapv #(gen-symbol %1 global?) ret-type)
                (gen-symbol ret-type global?)))
        ast {:ident (csnd-gensym)
             :opcode  opcode
             :in      (resolve-p in)
             :out     out
             :global? global?
             :operator operator
             :patch patch}]
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
  (letfn [(node-filter [n]
            (if *global*
              (filter isa-csound-type? n)
              (filter #(and (isa-csound-type? %)
                            (not (:global? %))) n)))]
    (loop [v [[(:ident root-node) root-node]]
           q (node-filter (:in root-node))]
      (if (empty? q)
        (reverse v)
        (let [children (mapv #(vector (:ident %) %) q)
              v (dedupe-nodes v children)
              v (into v children)
              q (flatten (map #(node-filter (:in (second %))) children))]
          (recur v q))))))

(defn inject-delayrw [tree whole-tree]
  (let [patch-ident (-> tree first second :patch :ident)
        [idelaytime audio-signal-to-write] (-> tree first second :patch :in)
        parsed-audio-signal-to-write (if (some #(= (:ident audio-signal-to-write) (first %)) whole-tree)
                                       [] (flatten-tree audio-signal-to-write))
        ;; Deduplicate here the parsed-audio-signal-to-write?
        delayr-ident (csnd-gensym)
        delayw-ident (csnd-gensym)
        new-tree (into [[delayr-ident (new AudioSignal (ast-node 'AudioSignal "delayr" [idelaytime] *global*))]]
                       parsed-audio-signal-to-write)]
    (loop [tree tree
           new-tree new-tree
           cur-index (inc (count new-tree))
           last-tap (inc (count new-tree))]
      (if (empty? tree)
        (into
         (into (subvec new-tree 0 last-tap)
               [[delayw-ident (new IO (ast-node 'IO "delayw" [audio-signal-to-write] *global*))]])
         (subvec new-tree last-tap))
        (let [cur-node (first tree)
              matching-tap? (= patch-ident (-> cur-node second :patch :ident))
              new-node (if-not matching-tap? cur-node
                               [(first cur-node) (empty (second cur-node))])]
          (recur (rest tree)
                 (conj new-tree new-node)
                 (inc cur-index)
                 (if matching-tap?
                   cur-index
                   last-tap)))))))

(defn inject-patches [whole-tree]
  (loop [tree whole-tree
         patched-tree []]
    (if (empty? tree)
      patched-tree
      (let [node (first tree)
            [_ ast] node
            ;; _ (prn "opc: " (:opcode ast) (:ident ast))
            patch (:patch (:patch ast))
            tree (case patch
                   nil tree
                   :delayrw (inject-delayrw tree whole-tree))]
        (if (nil? patch)
          (recur
           (rest tree)
           (conj patched-tree node))
          (recur tree
                 patched-tree))))))

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

(defn tree-to-assembler [tree]
  (-> tree
      flatten-tree
      inject-patches
      parse-tree))

(defn global-tree-to-out [tree]
  (let [out-str (-> tree
                    flatten-tree
                    inject-patches
                    parse-tree
                    parse-to-string)]
    (when (:connection @conn/connection)
      ((:compile-orc-fn @conn/connection) out-str))
    tree))

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



#_(:num (last (reduce (fn [i p]
                        (conj i p (new ScoreParameter (+ 4 (/ (count i) 2))))) [] '[a b c])))

#_(println (parse-to-string  (asig1)))


;; (load-file "src/csound/opcodes.cljs")


(comment
  (defn poscil [amp cps & [ifn]]
    (let [out-types-quoted 'AudioSignal
          out-types AudioSignal
          ast (ast-node out-types-quoted
                        "poscil"
                        [amp cps ifn]
                        false)]
      (new out-types ast)))

  (def asig1 (poscil 1 (ScoreParameter. 4)))
  
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
