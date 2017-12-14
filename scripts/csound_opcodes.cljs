(ns scripts.csound_opcodes
  (:require [fs]
            [lumo.io :as io]
            [clojure.string :as string]
            [scripts.metadata :as m]
            [goog.string :as gstring]
            goog.string.format))


(def sliders #(re-find #"^slider*" (first %)))


(def csound-opcode-list
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
       vec))

(defn find-specs [oname]
  (filter #(= (first %) oname)
          csound-opcode-list))

(defn poscil
  {:arglists '([p1 p2 p3 p4] [i1 i2 i3 i4])}
  [& [p1 p2 p3 p4 & rest :as env]]
  [rest env])

(poscil 1 2 3 2 3)


(defn skeleton [oname arglists pnames opt-pnames]
  (let [arglists (or arglists "([])")]
    (gstring/format "(defn %s
  {:arglists '%s}
  %s
  (fn [ctx] env))
" oname arglists pnames)))


(def fdef-spec-fn
  {"a" "valid-ar?"
   "k" "valid-kr?"
   "i" "valid-i?"
   "S" "valid-S?"})

(defn fdef-skeleton [oname op-symbol fdef-data]
  (let [multi-dispatch? (< 1 (count fdef-data))
        alt-p1 (if multi-dispatch?
                 "(s/alt\n" "(s/cat ")
        indent "         "
        p2 (if multi-dispatch?
             (apply str
                    (for [dispatch fdef-data]
                      (let [dispatch-name-key (keyword (apply str (map first dispatch)))
                            arg-seq (->> dispatch
                                         (map #(str (keyword (second %)) " "
                                                    (get fdef-spec-fn (first %))
                                                    (when (nth % 2) "*") " "))
                                         (apply str))
                            arg-set (str arg-seq ")")]
                        (str indent dispatch-name-key " (s/cat " arg-seq ")\n"))))
             (->> (first fdef-data)
                  (map #(str (keyword (second %)) " "
                             (get fdef-spec-fn (first %))
                             (when (nth % 2) "*") " "))
                  (apply str)))
        _ (prn fdef-data)
        spec (str alt-p1 p2 (if multi-dispatch? "))" ")"))]
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
   "a[]" "a[]"
   "m" "a"
   "S" "S"
   "S[]" "S[]"
   "x" "x"
   "*" "x"
   "k" "k"
   "k[]" "k[]"
   "O" "k"
   "P" "k"
   "V" "k"
   "z" "k"
   "f" "f"
   "i" "i"
   "v" "i"
   "i[]" "i[]"
   "j" "i"
   "o" "i"
   "p" "i"
   "." "any_rate_"
   "l" "l"
   })

(def rate-to-type
  {"a"   "AudioSignal"
   "k"   "ControlSignal"
   "f"   "SpectralSignal"
   "i"   "Variable"
   "S"   "String"
   "a[]" "AudioArray"
   "k[]" "ControlArray"
   "i[]" "VariableArray"
   ""    "IO"
   nil   "IO"})

(defn pname-rename [pname]
  (case pname
    "fn" "table"
    "phs" "phase"
    "nsnum" "insnum"
    pname))

(defn prn-vector [v]
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
             arg-num 1]
        (if (empty? in-specs)
          [(prn-vector arglists)
           (prn-vector (if opt-arg?
                         (conj pnames "]")
                         pnames))
           fdef-data]
          (let [in-spec (first in-specs)
                this-star-arg? (= "*" in-spec)
                in-spec (if this-star-arg? "x" in-spec)
                in-specs (if this-star-arg?
                           (into (remove #(= "*" %) in-specs) (take 16 (cycle ["x"])))
                           in-specs)
                ;; _ (prn in-specs)
                opt-arg (get opt-inargs in-spec)
                ;; Opt arg can have no default value (nil)
                this-opt-arg? (some #(= in-spec %) (keys opt-inargs))
                meta (first metas)
                meta (if (re-find #"/" meta) nil meta)
                pname (pname-rename (if meta (subs meta 1) (str "arg" arg-num)))
                argtype (get intype-to-rate in-spec)
                _ (prn in-spec  pname)
                argname (str (subs argtype 0 1) pname (subs argtype 1))]
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
                   (conj fdef-data [argtype pname this-opt-arg? this-star-arg?])
                   (if (and this-opt-arg? (not opt-arg?))
                     this-opt-arg?
                     opt-arg?)
                   (if (and this-star-arg? (not star-arg?))
                     this-star-arg? star-arg?)
                   (inc arg-num))))))))

(defn resolve-out-symbols [opcode spec]
  (let [in-spec (second (first spec))
        default {(ffirst spec) spec}]
    (cond
      (= "(null)" in-spec)
      ;; (assoc default :mutates true)
      :mutates
      (< 1 (count (split-specs in-spec)))
      default
      :else
      (let [out-rates (->> spec
                           (map #(second %))
                           dedupe
                           (map #(get intype-to-rate %)))]
        (reduce (fn [m r]
                  (let [rate (if (re-find #"\[" r)
                               (str (string/replace r "[]" "") "arr"))]
                    (assoc m (str opcode ":" rate)
                           (filter (fn [s] (or (= r (second s))
                                               (= rate (second s)))) spec)))) default out-rates)))))

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
              out-meta (get-in @m/metadata-db [opcode :out])
              args (map #(parse-in-args % in-meta) (or (get all-out op-symbol)
                                                       specs))
              arglists (str "(" (apply str (interpose " " (map first args))) ")")
              pnames (second (first args))
              fdef-data (map #(nth % 2) args)
              fdef-str (if (empty? fdef-data)
                         ""
                         (fdef-skeleton opcode op-symbol fdef-data))
              ;; _ (prn fdef-data)
              opt-pnames ""
              out-str (str out-str "\n"
                           (skeleton op-symbol arglists pnames opt-pnames))]
          (recur (rest op-symbols)
                 (str out-str "\n"
                      fdef-str)))))))

(defn debug [opc]
  (loop [opcodes ;;[opc]
         (take 1 (keys @m/metadata-db))
         out-str ""]
    (if (empty? opcodes)
      (println out-str)
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

(find-specs "poscil")
(get @m/metadata-db "xout")


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
                           ["poscil" "k" "kkjo"]))
