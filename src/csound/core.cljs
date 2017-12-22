(ns csound.core
  (:require [cljs.spec.alpha :as s]
            [cljs.spec.test.alpha :as stest]
            [clojure.zip :as zip]
            [clojure.walk :as walk]
            [clojure.string :as string]
            [csound.connection :as conn])
  (:require-macros
   [csound.macros :refer [definstr]]))


(def ^:dynamic *0dbfs* 1)
(def ^:dynamic *nchnls* 2)
(def ^:dynamic *sr* 44100)
(def ^:dynamic *ksmps* 16)
(def ^:dynamic *A4* 440)
(def ^:dynamic *global* nil)


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
     StringArray (str "S" (csnd-gensym "0") "[]"))))

(declare tree-to-assembler
         global-tree-to-out)

;; (-attachBoolean (AudioSignal. {:boolean 1}) 666 777)

;; (-attachBoolean {:boolean 1} 666 777)

(defprotocol ICsoundBool
  (-getBoolean     [this])
  (-getBooleanCase [this])
  (-attachBoolean  [this bool-object bool-case]))

(defn regenerate-out-symbols [this]
  (let [ret (:ret-type-quoted this)]
    (if (seqable? ret)
      (mapv #(gen-symbol % (:global? this)) ret)
      (gen-symbol ret (:global? this)))))

(deftype AudioSignal [ast]
  ICsoundBool
  (-getBoolean [this]
    (:boolean this))
  (-getBooleanCase [this]
    (:boolean-case this))
  (-attachBoolean [this bool-object bool-case]
    (new AudioSignal (assoc (.-ast this) :boolean bool-object :boolean-case bool-case
                            :ident (csnd-gensym) :out (regenerate-out-symbols this))))
  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found] (get (.-ast coll) k not-found))
  IFn
  (-invoke [coll] (tree-to-assembler (.-ast coll)))
  (-invoke [coll assembler] (into (tree-to-assembler (.-ast coll)) assembler))
  IEmptyableCollection
  (-empty [coll] (new AudioSignal (dissoc (.-ast coll) :patch))))

(deftype ControlSignal [ast]
  ICsoundBool
  (-getBoolean [this]
    (:boolean this))
  (-getBooleanCase [this]
    (:boolean-case this))
  (-attachBoolean [this bool-object bool-case]
    (new AudioSignal (assoc (.-ast this) :boolean bool-object :boolean-case bool-case
                            :ident (csnd-gensym) :out (regenerate-out-symbols this))))
  ILookup
  (-lookup [coll k not-found] (get (.-ast coll) k not-found))
  (-lookup [coll k] (get (.-ast coll) k))
  IFn
  (-invoke [coll] (tree-to-assembler (.-ast coll)))
  (-invoke [coll assembler] (into (tree-to-assembler (.-ast coll)) assembler))
  IEmptyableCollection
  (-empty [coll] (new ControlSignal (dissoc (.-ast coll) :patch))))

(deftype FrequencySignal [ast]
  ICsoundBool
  (-getBoolean [this]
    (:boolean this))
  (-getBooleanCase [this]
    (:boolean-case this))
  (-attachBoolean [this bool-object bool-case]
    (new AudioSignal (assoc (.-ast this) :boolean bool-object :boolean-case bool-case
                            :ident (csnd-gensym) :out (regenerate-out-symbols this))))
  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found] (get (.-ast coll) k not-found))
  IFn
  (-invoke [coll] (tree-to-assembler (.-ast coll)))
  (-invoke [coll assembler] (into (tree-to-assembler (.-ast coll)) assembler))
  IEmptyableCollection
  (-empty [coll] (new FrequencySignal (dissoc (.-ast coll) :patch))))

(deftype Variable [ast]
  ICsoundBool
  (-getBoolean [this]
    (:boolean this))
  (-getBooleanCase [this]
    (:boolean-case this))
  (-attachBoolean [this bool-object bool-case]
    (new AudioSignal (assoc (.-ast this) :boolean bool-object :boolean-case bool-case
                            :ident (csnd-gensym) :out (regenerate-out-symbols this))))
  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found] (get (.-ast coll) k not-found))
  IFn
  (-invoke [coll] (tree-to-assembler (.-ast coll)))
  (-invoke [coll assembler] (into (tree-to-assembler (.-ast coll)) assembler))
  IEmptyableCollection
  (-empty [coll] (new Variable (dissoc (.-ast coll) :patch))))

(deftype String [ast]
  ICsoundBool
  (-getBoolean [this]
    (:boolean this))
  (-getBooleanCase [this]
    (:boolean-case this))
  (-attachBoolean [this bool-object bool-case]
    (new AudioSignal (assoc (.-ast this) :boolean bool-object :boolean-case bool-case
                            :ident (csnd-gensym) :out (regenerate-out-symbols this))))
  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found] (get (.-ast coll) k not-found))
  IFn
  (-invoke [coll] (tree-to-assembler (.-ast coll)))
  (-invoke [coll assembler] (into (tree-to-assembler (.-ast coll)) assembler))
  IEmptyableCollection
  (-empty [coll] (new String (dissoc (.-ast coll) :patch))))

(deftype AudioArray [ast]
  ICsoundBool
  (-getBoolean [this]
    (:boolean this))
  (-getBooleanCase [this]
    (:boolean-case this))
  (-attachBoolean [this bool-object bool-case]
    (new AudioSignal (assoc (.-ast this) :boolean bool-object :boolean-case bool-case
                            :ident (csnd-gensym) :out (regenerate-out-symbols this))))
  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found] (get (.-ast coll) k not-found))
  IFn
  (-invoke [coll] (tree-to-assembler (.-ast coll)))
  (-invoke [coll assembler] (into (tree-to-assembler (.-ast coll)) assembler))
  IEmptyableCollection
  (-empty [coll] (new AudioArray (dissoc (.-ast coll) :patch))))

(deftype ControlArray [ast]
  ICsoundBool
  (-getBoolean [this]
    (:boolean this))
  (-getBooleanCase [this]
    (:boolean-case this))
  (-attachBoolean [this bool-object bool-case]
    (new AudioSignal (assoc (.-ast this) :boolean bool-object :boolean-case bool-case
                            :ident (csnd-gensym) :out (regenerate-out-symbols this))))
  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found] (get (.-ast coll) k not-found))
  IFn
  (-invoke [coll] (tree-to-assembler (.-ast coll)))
  (-invoke [coll assembler] (into (tree-to-assembler (.-ast coll)) assembler))
  IEmptyableCollection
  (-empty [coll] (new ControlArray (dissoc (.-ast coll) :patch))))

(deftype VariableArray [ast]
  ICsoundBool
  (-getBoolean [this]
    (:boolean this))
  (-getBooleanCase [this]
    (:boolean-case this))
  (-attachBoolean [this bool-object bool-case]
    (new AudioSignal (assoc (.-ast this) :boolean bool-object :boolean-case bool-case
                            :ident (csnd-gensym) :out (regenerate-out-symbols this))))
  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found] (get (.-ast coll) k not-found))
  IFn
  (-invoke [coll] (tree-to-assembler (.-ast coll)))
  (-invoke [coll assembler] (into (tree-to-assembler (.-ast coll)) assembler))
  IEmptyableCollection
  (-empty [coll] (new VariableArray (dissoc (.-ast coll) :patch))))

(deftype StringArray [ast]
  ICsoundBool
  (-getBoolean [this]
    (:boolean this))
  (-getBooleanCase [this]
    (:boolean-case this))
  (-attachBoolean [this bool-object bool-case]
    (new AudioSignal (assoc (.-ast this) :boolean bool-object :boolean-case bool-case
                            :ident (csnd-gensym) :out (regenerate-out-symbols this))))
  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found] (get (.-ast coll) k not-found))
  IFn
  (-invoke [coll] (tree-to-assembler (.-ast coll)))
  (-invoke [coll assembler] (into (tree-to-assembler (.-ast coll)) assembler))
  IEmptyableCollection
  (-empty [coll] (new StringArray (dissoc (.-ast coll) :patch))))

(deftype IO [ast]
  ICsoundBool
  (-getBoolean [this]
    (:boolean this))
  (-getBooleanCase [this]
    (:boolean-case this))
  (-attachBoolean [this bool-object bool-case]
    (new AudioSignal (assoc (.-ast this) :boolean bool-object :boolean-case bool-case
                            :ident (csnd-gensym) :out (regenerate-out-symbols this))))
  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found] (get (.-ast coll) k not-found))
  IFn
  (-invoke [coll] (tree-to-assembler (.-ast coll)))
  (-invoke [coll assembler] (into (tree-to-assembler (.-ast coll)) assembler))
  IEmptyableCollection
  (-empty [coll] (new IO (dissoc (.-ast coll) :patch))))

(deftype ScoreParameter [p-num]
  ICsoundBool
  (-getBoolean [this]
    (:boolean this))
  (-getBooleanCase [this]
    (:boolean-case this))
  (-attachBoolean [this bool-object bool-case]
    (new AudioSignal (assoc (.-ast this) :boolean bool-object :boolean-case bool-case
                            :ident (csnd-gensym) :out (regenerate-out-symbols this))))
  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found] (str "p" (.-p-num coll)))
  IFn
  (-invoke [coll] (str "p" (.-p-num coll))))

(deftype CsoundBoolean [ast]
  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found] (get (.-ast coll) k not-found))
  IFn
  (-invoke [coll] (tree-to-assembler (.-ast coll)))
  (-invoke [coll assembler] (into (tree-to-assembler (.-ast coll)) assembler)))

(deftype CsoundComparator [ast]
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
        out (if (#{'CsoundBoolean 'CsoundComparator 'IO} ret-type)
              nil
              (if (seqable? ret-type)
                (mapv #(gen-symbol %1 global?) ret-type)
                (gen-symbol ret-type global?)))
        ast {:ident (csnd-gensym)
             :opcode  opcode
             :in      (resolve-p in)
             :out     out
             :ret-type-quoted ret-type
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
      ;; (prn (map #(.-ast (second %)) v))
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


(defn parse-comparator [object]
  (str "(" (reduce (fn [s in]
                     (cond (instance? CsoundComparator in)
                           (str s (parse-comparator in))
                           (instance? ScoreParameter in)
                           (str s (when-not (empty? s) (:operator object)) (in))
                           (isa-csound-type? in)
                           (str s (when-not (empty? s) (:operator object)) (:out in))
                           :else
                           (str s (when-not (empty? s) (:operator object)) in)))
                   "" (:in object)) ")"))


(defn parse-booleans [patched-tree object]
  (let [bool-object (:boolean object)
        [comparator-object & cases] (:in bool-object)
        ;; Reolve possible "hidden" objects in comparator object
        patched-tree (into patched-tree
                           (->> (filter isa-csound-type? (:in comparator-object))
                                vec
                                (mapv flatten-tree)
                                (apply into)))
        if-node [(:ident bool-object)
                 (str "if " (parse-comparator comparator-object) " "
                      (:opcode bool-object) " " (:ident bool-object) "_case_0"
                      (str "\n" (:opcode bool-object) " " (:ident bool-object) "_case_1"))]
        endif-node [(str (:ident bool-object) "_endif")
                    (str (:ident bool-object) "_endif:")]
        patched-tree (conj patched-tree if-node)
        patched-tree (into patched-tree
                           (apply into
                                  (mapv (fn [obj case-index]
                                          [(vector (str (:ident bool-object) "_case_" case-index)
                                                   (str (:ident bool-object) "_case_" case-index ":"))
                                           (vector (:ident obj) obj)
                                           (vector (str (:ident bool-object) "_endif_" case-index)
                                                   (str (:opcode bool-object) " "
                                                        (:ident bool-object) "_endif"))])
                                        cases (range))))
        patched-tree (conj patched-tree endif-node)]
    ;; (prn bool-object)
    ;; (throw (js/Error. "nomean"))
    patched-tree))


(defn post-process-tree
  "Inject patches"
  [whole-tree]
  (loop [tree whole-tree
         patched-tree []
         resolved-bools #{}]
    (if (empty? tree)
      patched-tree
      (let [node (first tree)
            [ident object] node
            ;; _ (prn "node" node)
            patch (:patch (:patch object))
            tree (case patch
                   nil tree
                   :delayrw (inject-delayrw tree whole-tree))
            unexpanded-bool? (and (contains? object :boolean)
                                  (not (resolved-bools ident)))
            patched-tree (if unexpanded-bool?
                           (parse-booleans patched-tree object)
                           patched-tree)]
        (cond
          patch
          (recur tree
                 patched-tree
                 resolved-bools)
          unexpanded-bool?
          (recur (rest tree)
                 patched-tree
                 (conj resolved-bools ident))
          :else
          (recur
           (rest tree)
           (conj patched-tree node)
           resolved-bools))))))

(defn parse-tree [tree]
  (-> (fn [acc node]
        (let [[ident object] node]
          (if (string? object)
            (conj acc node)
            (let [parent-node (if (contains? object :parent) (:parent object) object)
                  out (:out parent-node)
                  out (if (coll? out) (apply str (interpose ", " out)) out)
                  operator (if-let [operator (:operator object)] (str " " operator " ") ", ")]
              (conj acc [(:ident parent-node)
                         (apply str (interpose " "
                                               (cond->> [(:opcode parent-node)
                                                         (->> (map #(or (:out %) %)
                                                                   (remove nil? (:in object)))
                                                              (interpose operator)
                                                              (apply str))]
                                                 out (into [out]))))])))))
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
      post-process-tree
      parse-tree))

(defn global-tree-to-out [tree]
  (let [out-str (-> tree
                    flatten-tree
                    post-process-tree
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
