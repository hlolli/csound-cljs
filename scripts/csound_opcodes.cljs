(ns scripts.csound_opcodes
  (:require [fs]
            [lumo.io :as io]
            [clojure.string :as string]
            [csound.metadata :as m]
            [goog.string :as gstring]
            goog.string.format))


;; All rates
(defn- handle-x [])

;; z varag: k-rate 
(defn- handle-z [])

;; m vararg: a-rate
(defn- handle-m [])

;; o opt-arg: 1-rate default: 0
(defn- handle-o [])

;; all rates and opt-args
(defn- handle-star [])

(fs/readdirSync "")

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
       vec))

(defn find-specs [oname]
  (filter #(= (first %) oname)
          csound-opcode-list))

(defn poscil
  {:arglists '([p1 p2 p3 p4] [i1 i2 i3 i4])}
  [& [p1 p2 p3 p4 & rest :as env]]
  [rest env])

(poscil 1 2 3 2 3)


(defn skeleton [& [oname arglists pnames]]
  (let [arglists (or arglists "([])")]
    (gstring/format "(defn %s
  {:arglists '%s}
  [& %s ]
  (fn [ctx] env))
" oname arglists pnames)))


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
   "i[]" "i[]"
   "j" "i"
   "o" "i"
   "p" "i"
   "." "any_rate_"
   "l" "l"
   })

(def rate-to-type
  {"a"   "CsoundAudioSignal"
   "k"   "CsoundControlSignal"
   "f"   "CsoundSpectralSignal"
   "i"   "CsoundVariable"
   "S"   "CsoundString"
   "a[]" "CsoundAudioArray"
   "k[]" "CsoundControlArray"
   "i[]" "CsoundVariableArray"})

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
      [nil nil]
      (loop [in-specs (split-specs in-specs)
             metas in-meta
             arglists []
             pnames []
             opt-arg? false]
        (if (empty? in-specs)
          [(prn-vector arglists)
           (prn-vector (apply conj pnames [":as" "env"]))]
          (let [in-spec (first in-specs)
                opt-arg? (some #(= in-spec %) (keys opt-inargs))
                meta (first metas)
                pname (str (pname-rename (if meta (subs meta 1) "arg"))
                           (when opt-arg? "*"))
                argtype (get intype-to-rate in-spec)
                argname (str (subs argtype 0 1) pname (subs argtype 1))]
            (recur (rest in-specs)
                   (if (< 1 (count metas))
                     (rest metas)
                     metas)
                   (conj arglists argname)
                   (conj pnames pname)
                   opt-arg?)))))))

(parse-in-args ["poscil" "a" "aajo"]
               ;; ["poscil" "a" "akjo"]
               ;; ["poscil" "a" "kajo"]
               ;; ["poscil" "a" "kkjo"]
               ;; ["poscil" "k" "kkjo"]
               ["kamp" "kcps" "ifn" "iphs"])


(resolve-out-types "poscil"
                   (list ["poscil" "a" "aajo"]
                         ["poscil" "a" "akjo"]
                         ["poscil" "a" "kajo"]
                         ["poscil" "a" "kkjo"]
                         ["poscil" "k" "kkjo"]))

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
                  (assoc m (str opcode ":" r)
                         (filter (fn [s] (= r (second s))) spec))) default out-rates)))))

(defn parse-entry [out-str all-out specs opcode]
  (let [out-symbols (if (= :mutates all-out)
                      [opcode]
                      (keys all-out))]
    ;; (prn out-symbols)
    (loop [op-symbols out-symbols
           out-str out-str]
      (if (empty? op-symbols)
        out-str
        (let [op-symbol (first op-symbols)
              in-meta  (get-in @m/metadata-db [opcode :in])
              out-meta (get-in @m/metadata-db [opcode :out])
              args (map #(parse-in-args % in-meta) (or (get all-out op-symbol)
                                                       specs))
              _ (prn (get all-out op-symbol) specs)
              arglists (str "(" (apply str (interpose " " (map first args))) ")")
              pnames (second (first args))
              out-str (str out-str "\n"
                           (skeleton op-symbol arglists pnames))]
          (recur (rest op-symbols)
                 out-str))))))

(loop [opcodes ["xout"] ;;(keys @m/metadata-db)
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
             out-str))))

(find-specs "poscil")
(get @m/metadata-db "xout")


(#{:a :b} :a)
