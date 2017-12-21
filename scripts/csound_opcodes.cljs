(ns scripts.csound_opcodes
  (:require [fs]
            [lumo.io :as io]
            [clojure.string :as string]
            [scripts.metadata :as m]
            [goog.string :as gstring]
            goog.string.format))


(def csound-opcode-list
  (->
   (->> (for [line (-> (io/slurp "scripts/csound_raw_spec.txt")
                       string/split-lines)]
          
          (-> line
              (string/replace #"\s+" " ")
              (string/split #"\s")))
        (remove #(m/fl-opcodes (first %)))
        (remove #(re-find #"^slider*" (first %)))
        (remove #(m/deprecated (first %)))
        (remove #(m/operators (first %)))
        (remove #(m/redundants (first %)))
        ;; Use rather in defopcode
        (remove #(= "xout" (first %)))
        (remove #(= "xin" (first %)))
        (remove #(= "framebuffer" (first %)))
        (remove #(= "delayr" (first %)))
        (remove #(= "strcpy" (first %)))
        (remove #(= "sprintf" (first %)))
        (remove #(= "fsprintks" (first %)))
        (remove #(= "changed2" (first %)))
        (remove #(= "tablexseg" (first %)))
        (remove #(= "printf" (first %)))
        (remove #(= "printf_i" (first %)))
        (remove #(= "sockrecv" (first %)))
        (remove #(= "lenarray" (first %)))
        (remove #(= "delayr" (first %)))
        (remove #(= "vdel_k" (first %)))
        (remove #(= "init" (first %)))
        (remove #(= "slicearray" (first %)))
        (remove #(= "in" (first %)))
        (remove #(= "taninv" (first %)))
        vec)
   (conj ["framebuffer" "k[]" "ai"]
         ["framebuffer" "a" "ki"]
         ["delayr" "a" "io"]
         ["strcpy" "S" "S"]
         ["sprintf" "S" "S*"]
         ["fprintks" "(null)" "SSN"]
         ["fprintks" "(null)" "iSN"]
         ["changed2" "k" "i[]"]
         ["changed2" "k" "a[]"]
         ["changed2" "k" "S[]"]
         ["changed2" "k" "z"]
         ["tablexseg" "(null)" "iim"]
         ["printf" "(null)" "Sz"]
         ["printf_i" "(null)" "Sm"]
         ["sockrecv" "a" "ii"]
         ["lenarray" "i" "i[]p"]
         ["lenarray" "k" "k[]p"]
         ["delayr" "k" "kio"]
         ["vdel_k" "k" "kkio"]
         ["init" "i[]" "m"]
         ["init" "a[]" "m"]
         ["init" "S[]" "m"]
         ["init" "i" "i"]
         ["init" "S" "S"]
         ["init" "S" "i"]
         ["init" "f" "f"]
         ["init" "a" "i"]
         ["init" "k" "i"]
         ["slicearray" "a[]" "a[]iip"]
         ["slicearray" "k[]" "k[]iip"]
         ["slicearray" "S[]" "S[]iip"]
         ["slicearray" "i[]" "i[]iip"]
         ["in" "a" "(null)"]
         ["in" "a[]" "(null)"]
         ["taninv" "a" "a"]
         ["taninv" "i" "i"]
         ["taninv" "i[]" "i[]"]
         ["taninv" "k" "k"]
         ["taninv" "k[]" "k[]"]
         )))

(defn find-specs [oname]
  (filter #(= (first %) oname)
          csound-opcode-list))

#_(defn poscil
    {:arglists '([p1 p2 p3 p4] [i1 i2 i3 i4])}
    [& [p1 p2 p3 p4 & rest :as env]]
    [rest env])

;; (poscil 1 2 3 2 3)

(defn remove-spec-duplicate
  [spec-list]
  (-> (fn [acc s]
        (if (some #(= (apply str (map first %))
                      (apply str (map first s))) acc)
          acc (conj acc s)))
      (reduce '() spec-list)))

(def fdef-spec-fn
  {"a" "valid-ar?"
   "k" "valid-kr?"
   "i" "valid-i?"
   "S" "valid-S?"
   "x" "valid-x?"
   "f" "valid-f?"
   "aArr" "valid-aArr?"
   "kArr" "valid-kArr?"
   "iArr" "valid-iArr?"
   "SArr" "valid-SArr?"})

(defn fdef-skeleton [oname op-symbol fdef-data]
  (let [multi-dispatch? (< 1 (count fdef-data))
        alt-p1 (if multi-dispatch?
                 "(s/alt\n" "(s/cat ")
        indent "         "
        fdef-data-deduped-in (remove-spec-duplicate fdef-data)
        p2 (if multi-dispatch?
             (apply str
                    (for [dispatch fdef-data-deduped-in]
                      (let [dispatch-name-key (keyword (apply str (map first dispatch)))
                            arg-seq (->> dispatch
                                         (map #(str (keyword (second %)) " "
                                                    (when (nth % 2) "(s/? ")
                                                    (get fdef-spec-fn (first %))
                                                    (when (nth % 2) "*)") " "))
                                         (apply str))
                            arg-set (str arg-seq ")")]
                        (str indent dispatch-name-key " (s/cat " arg-seq ")\n"))))
             (->> (first fdef-data)
                  (map #(str (keyword (second %)) " "
                             (when (nth % 2) "(s/? ")
                             (get fdef-spec-fn (first %))
                             (when (nth % 2) "*)") " "))
                  (apply str)))
        spec (str alt-p1 p2 (if multi-dispatch? (str indent "))") "))"))]
    (gstring/format "(s/fdef %s
  :args %s
(stest/instrument `%s)\n" op-symbol spec op-symbol)))


(def opt-inargs
  {"j" -1
   "O" 0
   "P" 1
   "V" 0.5
   "o" 0
   "p" 1
   "q" 10
   "h" 127
   "J" -1
   "?" nil
   "*" nil
   "W" :inf ;; s-rate
   "y" :inf ;; a-rate
   "z" :inf ;; k-rate
   "Z" :inf ;; kaka...
   "m" :inf ;; i-rate
   "M" :inf ;; all rates exluding S
   "N" :inf ;; all rates including S
   })

(def intype-to-rate
  {"a" "a"
   "a[]" "aArr"
   "S" "S"
   "W" "S"
   "S[]" "SArr"
   "x" "x"
   "*" "x"
   ;; Fix M and N
   "M" "x"
   "N" "x"
   "k" "k"
   "k[]" "kArr"
   "O" "k"
   "P" "k"
   "V" "k"
   "z" "k"
   "J" "k"
   "f" "f"
   "i" "i"
   "m" "i"
   "h" "i"
   "v" "i"
   "q" "i"
   "i[]" "iArr"
   "j" "i"
   "o" "i"
   "p" "i"
   "." "any_rate_"
   "l" "l"
   "y" "a"
   "Z" ["a" "k"]
   })


(def out-rate-to-type
  {"a"   "AudioSignal"
   "m"   "AudioSignal"
   "k"   "ControlSignal"
   "z"   "ControlSignal"
   "f"   "FrequencySignal"
   "i"   "Variable"
   "S"   "String"
   "S[]" "StringArray"
   "a[]" "AudioArray"
   "k[]" "ControlArray"
   "i[]" "VariableArray"
   ""    "IO"
   nil   "IO"})

(defn vector->str [v]
  (str "["
       (->> (interpose " " v)
            (apply str))
       "]"))

(defn split-specs [specs]
  (reduce (fn [acc s]
            (if (or (= "[" s) (= "]" s))
              (conj (subvec acc 0 (dec (count acc))) (str (last acc) s))
              (conj acc s)))
          []
          (vec (seq specs))))

(defn skeleton [oname arglists pnames out-types command]
  (let [arglists (string/trim (or arglists "([])"))
        [out-types out-cnt] (if (= "(null)" out-types)
                              ["IO" 0]
                              (let [rates (split-specs out-types)
                                    out-cnt (count rates)]
                                (if (< 1 out-cnt)
                                  [(vector->str (mapv #(get out-rate-to-type %) rates)) out-cnt]
                                  [(get out-rate-to-type (first rates)) out-cnt])))
        parameters (-> pnames
                       (string/replace #"\[|\]|&" "")
                       (string/replace #"\s+" " ")
                       (string/trim))
        return (when out-types
                 (if (< 1 out-cnt)
                   "(mapv #(new %1 %2) out-types ast)"
                   "(new out-types ast)"))]
    (apply gstring/format "(defn %s
  {:arglists '%s}
  %s
  (let [out-types-quoted '%s
        out-types %s
        ast (ast-node out-types-quoted
                      \"%s\"
                      [%s]
                      *global*)]
    %s))
" [oname arglists pnames
   out-types out-types command
   parameters return])))

(defn pname-rename [pname]
  (case pname
    "fn" "table"
    "phs" "phase"
    "nsnum" "insnum"
    "bw" "bandwidth"
    "scl" "scale"
    "cf" "center-frequency"
    "fco" "cutoff-frequency"
    "res" "resonance"
    "dist" "distortion"
    "ndx" "index"
    pname))

(defn parse-in-args [specs in-meta]
  (let [[_ _ in-specs] specs]
    (if (= "(null)" in-specs)
      [nil "[]" []]
      (loop [in-specs (split-specs in-specs)
             metas in-meta
             arglists []
             pnames []
             fdef-data []
             opt-arg? false
             star-arg? false
             Z-arg? false
             z-arg? false
             m-arg? false
             y-arg? false
             W-arg? false
             arg-num 1]
        (if (empty? in-specs)
          [(vector->str arglists)
           (vector->str (if opt-arg?
                          (conj pnames "]")
                          pnames))
           fdef-data]
          (let [in-spec (first in-specs)
                opt-arg (get opt-inargs in-spec)
                ;; Opt arg can have no default value (nil)
                this-opt-arg? (some #(= in-spec %) (keys opt-inargs))
                this-star-arg? (or (= "*" in-spec)
                                   (= "M" in-spec)
                                   (= "N" in-spec))
                this-Z-arg? (= "Z" in-spec)
                this-z-arg? (= "z" in-spec)
                this-m-arg? (= "m" in-spec)
                this-y-arg? (= "y" in-spec)
                this-W-arg? (= "W" in-spec)
                arg-num (if this-opt-arg?
                          1 arg-num)
                in-spec (cond this-star-arg? "x"
                              (or this-z-arg? this-Z-arg?) "k"
                              :else in-spec)
                in-specs (cond this-star-arg?
                               (into (remove #(or (= "*" %)
                                                  (= "M" %)
                                                  (= "N" %)) in-specs) (take 16 (cycle ["x"])))
                               this-Z-arg?
                               (into (remove #(= "Z" %) in-specs) (take 16 (cycle ["a" "k"])))
                               this-z-arg?
                               (into (remove #(= "z" %) in-specs) (take 16 (cycle ["k"])))
                               this-m-arg?
                               (into (remove #(= "m" %) in-specs) (take 16 (cycle ["i"])))
                               this-y-arg?
                               (into (remove #(= "y" %) in-specs) (take 32 (cycle ["a"])))
                               this-W-arg?
                               (into (remove #(= "W" %) in-specs) (take 16 (cycle ["S"])))
                               :else
                               in-specs)
                meta (first metas)
                meta (if (re-find #"/" meta) nil meta)
                ;; _ (prn "meta: " meta)
                pname (pname-rename (if (and meta
                                             (not (or this-Z-arg? Z-arg?))
                                             (not (re-find #"^[a-zA-Z][0-9]$" meta)))
                                      (let [numbered-meta? (re-find #"[0-9]$" meta)]
                                        ;; (prn meta)
                                        (cond
                                          numbered-meta? (str (string/replace (subs meta 1) #"[0-9]" "") arg-num)
                                          (or this-z-arg? this-Z-arg?
                                              z-arg? Z-arg?
                                              this-m-arg? m-arg?
                                              this-y-arg? y-arg?
                                              this-star-arg? star-arg?
                                              this-W-arg? W-arg?)
                                          (str (subs meta 1) arg-num)
                                          :else (subs meta 1)))
                                      (str "arg" arg-num)))
                pname (if (or opt-arg? this-opt-arg?) (str pname "*") pname)
                ;; _ (prn meta pname arg-num)
                argtype (get intype-to-rate in-spec)
                ;; _ (prn "argt: " in-spec argtype)
                argname (str (subs argtype 0 1) pname (subs argtype 1))
                ;; _ (prn meta pname argname)
                ]
            (recur (rest in-specs)
                   (if (< 1 (count metas))
                     (rest metas)
                     metas)
                   (if (and this-opt-arg? (not opt-arg?))
                     (conj arglists "&" argname)
                     (conj arglists argname))
                   (if (and this-opt-arg? (not opt-arg?))
                     (conj pnames "& [" pname)
                     (conj pnames pname))
                   (conj fdef-data [argtype pname
                                    (or opt-arg? this-opt-arg?)
                                    (or star-arg? this-star-arg?)])
                   (if (and this-opt-arg? (not opt-arg?))
                     this-opt-arg?
                     opt-arg?)
                   (if (and this-star-arg? (not star-arg?))
                     this-star-arg? star-arg?)
                   (if (and this-Z-arg? (not Z-arg?))
                     this-Z-arg? Z-arg?)
                   (if (and this-z-arg? (not z-arg?))
                     this-z-arg? z-arg?)
                   (if (and this-m-arg? (not m-arg?))
                     this-m-arg? m-arg?)
                   (if (and this-y-arg? (not y-arg?))
                     this-y-arg? y-arg?)
                   (if (and this-W-arg? (not W-arg?))
                     this-W-arg? W-arg?)
                   (inc arg-num))))))))

(defn resolve-out-symbols [opcode spec]
  (let [out-spec (second (first spec))
        default {(ffirst spec) spec}
        outtype-to-rate {"a" "a"
                         "a[]" "aArr"
                         "S" "S"
                         "S[]" "SArr"
                         "x" "x"
                         "*" "x"
                         "k" "k"
                         "f" "f"
                         "k[]" "kArr"
                         "m" "a"
                         "z" "k"
                         "i" "i"
                         "i[]" "iArr"}]
    ;; (prn out-spec)
    (cond
      (= "(null)" out-spec)
      ;; (assoc default :mutates true)
      :mutates
      (< 1 (count (split-specs out-spec)))
      default
      :else
      (let [out-rates (->> spec
                           (map #(second %))
                           dedupe
                           (map split-specs)
                           (map first)
                           (map #(get outtype-to-rate %))
                           )]
        ;; (prn out-rates)
        (if (< 1 (count out-rates))
          (reduce (fn [m r]
                    ;; (prn r spec)
                    (let [array? (re-find #"Arr" r)
                          some-vararg? (some #(re-find #"m|z|F" (second %)) spec)]
                      (assoc m (str opcode ":" r)
                             (filter (fn [s]
                                       (if array?
                                         (= (string/replace r "Arr" "[]") (second s))
                                         (if some-vararg?
                                           (case r
                                             "k" (or (= "z" (first (split-specs (second s))))
                                                     (= "k" (first (split-specs (second s)))))
                                             "f" (or (= "F" (first (split-specs (second s))))
                                                     (= "f" (first (split-specs (second s)))))
                                             "a" (or (= "m" (first (split-specs (second s))))
                                                     (= "a" (first (split-specs (second s)))))
                                             nil)
                                           (= r (second s))))) spec))))
                  default out-rates)
          default)))))

(defn parse-entry [out-str all-out specs opcode]
  (let [out-symbols (if (= :mutates all-out)
                      [opcode]
                      (keys all-out))]
    (loop [op-symbols out-symbols
           out-str out-str]
      (if (empty? op-symbols)
        out-str
        (let [op-symbol (first op-symbols)
              in-meta  (get-in @m/metadata-db [opcode :in])
              in-meta (->> in-meta
                           (remove #(= "..." %)))
              ;; _ (prn "inmeta" in-meta)
              out-meta (get-in @m/metadata-db [opcode :out])
              args (map #(parse-in-args % in-meta) (or (get all-out op-symbol)
                                                       specs))
              ;; _ (prn args)
              arglists (str "(" (apply str (interpose " " (map first args))) ")")
              pnames (second (first args))
              fdef-data (map #(nth % 2) args)
              fdef-data (if (seqable? fdef-data)
                          (remove empty? fdef-data)
                          fdef-data)
              fdef-str (if (empty? fdef-data)
                         ""
                         (fdef-skeleton opcode op-symbol fdef-data))
              ;; _ (prn "fdef-data" fdef-data fdef-str)
              out-types (first (distinct (map second (get all-out op-symbol))))
              ;; _ (prn (map second (get all-out op-symbol)))
              opt-pnames ""
              out-str (str out-str "\n"
                           (skeleton op-symbol arglists pnames out-types opcode))]
          (recur (rest op-symbols)
                 (str out-str "\n"
                      fdef-str)))))))

(defn generate-cljs []
  (loop [opcodes ;; ["taninv"]
         (keys @m/metadata-db)
         out-str ""]
    ;; (prn (first opcodes))
    (if (empty? opcodes)
      ;; (println out-str)
      (str
       ";; Autogenerated code, don't edit! ;;\n"
       "(ns csound.opcodes
  (:require [cljs.spec.alpha :as s]
            [cljs.spec.test.alpha :as stest]
            [csound.core :refer [*global*
                                 ast-node
                                 AudioSignal 
                                 ControlSignal
                                 FrequencySignal
                                 Variable String 
                                 AudioArray 
                                 ControlArray
                                 VariableArray 
                                 StringArray IO
                                 ScoreParameter
                                 valid-f? valid-f?*
                                 valid-x? valid-x?*
                                 valid-S? valid-S?*
                                 valid-SArr? valid-SArr?*
                                 valid-kArr? valid-kArr?*
                                 valid-ar? valid-ar?*
                                 valid-i? valid-i?*
                                 valid-kr? valid-kr?*
                                 valid-aArr? valid-aArr?*
                                 valid-iArr? valid-iArr?*]]))\n"
       out-str)
      (let [opcode (first opcodes)
            specs (find-specs opcode)
            ;; s is deprecated, usually represents k-rate sigs
            ;; "T" ["S" "i"]
            ;; "U" ["S" "k" "i"]
            specs (reduce (fn [acc v]
                            (let [expandable-vararg (re-find #"T|U" (nth v 2))
                                  v (assoc v 1 (string/replace (nth v 1) "s" "k"))]
                              (case expandable-vararg
                                "T" (conj acc (assoc v 2 (string/replace (nth v 2) "T" "i"))
                                          (assoc v 2 (string/replace (nth v 2) "T" "S")))
                                "U" (conj acc (assoc v 2 (string/replace (nth v 2) "U" "i"))
                                          (assoc v 2 (string/replace (nth v 2) "U" "k"))
                                          (assoc v 2 (string/replace (nth v 2) "U" "S")))
                                (conj acc v))))
                          [] specs)
            ;; _ (prn specs)
            all-out (resolve-out-symbols opcode specs)
            ;; _ (prn all-out)
            out-str (parse-entry out-str all-out specs opcode)]
        (recur (rest opcodes)
               out-str)))))

(comment
  
  (io/spit "src/csound/opcodes.cljs" (generate-cljs))
  
  (find-specs "init")
  (find-specs "taninv")
  (get @m/metadata-db "abs")
  (get @m/metadata-db "poscil")


  (parse-in-args ["poscil" "a" "aajo"]
                 ;; ["poscil" "a" "akjo"]
                 ;; ["poscil" "a" "kajo"]
                 ;; ["poscil" "a" "kkjo"]
                 ;; ["poscil" "k" "kkjo"]
                 ["kamp" "kcps" "ifn" "iphs"])


  (resolve-out-symbols "poscil"
                       (list ["poscil" "a" "aajo"]
                             ["poscil" "a" "akjo"]
                             ["poscil" "a" "kajo"]
                             ["poscil" "a" "kkjo"]
                             ["poscil" "k" "kkjo"])))
