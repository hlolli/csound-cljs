(ns scripts.metadata
  (:require [fs]
            [path]
            [xml2json :as xml]
            [clojure.string :as string]
            [clojure.zip :as zip]))

(def manual-opcodes-dir
  "/home/hlolli/csound/manual/opcodes/")

(def fl-opcodes (fn [file] (re-find #"^fl[^a]" file)))

(def deprecated #{"abetarand.xml" "abexprnd.xml"
                  "acauchy.xml" "aexprand.xml"
                  "agauss.xml" "agogobel.xml"
                  "alinrand.xml" "apcauchy.xml"
                  "apoisson.xml" "apow.xml" "atrirand.xml"
                  "aunirand.xml" "aweibull.xml" "clock.xml"
                  "ibetarand.xml" "ibexprnd.xml" "icauchy.xml"
                  "ictrl7.xml" "ictrl14.xml" "ictrl21.xml"
                  "iexprand.xml" "igauss.xml" "ilinrand.xml"
                  "imidic14.xml" "imidic7.xml" "imidic21.xml"
                  "instimek.xml" "instimes.xml" "ioff.xml" "ion.xml"
                  "iondur.xml" "iondur2.xml" "ioutat.xml"
                  "ioutc.xml" "ioutc14.xml" "ioutpb.xml"
                  "ioutpc.xml" "ioutpat.xml" "ipcauchy.xml"
                  "ipoisson.xml" "ipow.xml" "is16b14.xml"
                  "is32b14.xml" "islider16.xml" "islider32.xml"
                  "islider64.xml" "islider8.xml" "itablecopy.xml"
                  "itablegpw.xml" "itablemix.xml" "itablew.xml"
                  "itrirand.xml" "iunirand.xml" "iweibull.xml"
                  "kbetarand.xml" "kbexprnd.xml" "kcauchy.xml"
                  "kdump.xml" "kdump2.xml" "kdump3.xml"
                  "kdump4.xml" "kexprand.xml"})

(def operators #{"abs.xml" "modulus.xml"
                 "greaterthan.xml" "greaterequal.xml"
                 "lessthan.xml" "lessequal.xml"
                 "multiplies.xml" "adds.xml"
                 "subtracts.xml" "divides.xml"
                 "assign.xml" "plusbecomes.xml"
                 "equals.xml" "raises.xml"
                 "opor.xml" "opbitshl.xml"
                 "opbitshr.xml" "opbitand.xml"
                 "opbitor.xml" "opbitnot.xml"
                 "opnonequiv.xml" "if.xml"
                 "elseif.xml" "else.xml"
                 "endif.xml" "goto.xml" "igoto.xml"
                 "kgoto.xml" "tigoto.xml" "cigoto.xml"
                 "ckgoto.xml" "cggoto.xml" "timout.xml"
                 "kfilter2.xml" "kgauss.xml" "klinrand.xml"
                 "kon.xml" "koutat.xml" "koutc.xml"
                 "koutc14.xml" "koutpat.xml" "koutpb.xml"
                 "koutpc.xml" "kpcauchy.xml" "kpoisson.xml"
                 "kpow.xml" "kread.xml" "kread2.xml" "kread3.xml"
                 "kread4.xml" "ktrirand.xml" "kunirand.xml"
                 "kweibull.xml" "peakk.xml"})

(def redundants #{"convle.xml" "fiopen.xml" "oscilx.xml" "sense.xml"})

(def manual-opcode-files
  (->> (fs/readdirSync manual-opcodes-dir)
       js->clj
       (remove #(= "0dbfs.xml" %))
       (remove #(= "sr.xml" %))
       (remove #(= "kr.xml" %))
       (remove #(= "ksmps.xml" %))
       (remove #(= "nchnls.xml" %))
       (remove #(= "splitrig.txt" %))
       (remove #(= "A4.xml" %))
       (remove #(= "top.xml" %))
       (remove #(= "topXO.xml" %))
       (remove deprecated)
       (remove operators)
       (remove redundants)
       (remove fl-opcodes)))

(def metadata-db (atom {}))

(defn lookup-key [k coll]
  (let [coll-zip (zip/zipper coll? #(if (map? %) (vals %) %) nil coll)]
    (loop [x coll-zip]
      (when-not (zip/end? x)
        (if-let [v (-> x zip/node k)] v (recur (zip/next x)))))))


(doseq [ file manual-opcode-files
        ;; file ["hrtfer.xml"]
        ]
  (let [raw (fs/readFileSync (path/join manual-opcodes-dir file) "utf8")
        rem-ampersand (string/replace raw "&" "")
        command (-> (re-find #"<command>(.*)</command>" rem-ampersand)
                    second
                    string/trim)
        rem-command (-> rem-ampersand
                        (string/replace #"<[/]?command>" "")
                        (string/replace #"<[/]?quote>" "\""))
        all-xml (try (xml/toJson rem-command)
                     (catch js/Error e
                       (prn file command e)))
        json (.parse js/JSON all-xml)
        edn (js->clj json :keywordize-keys true)
        synopsis (let [s (lookup-key :synopsis edn)]
                   (if (vector? s)
                     (last s)
                     s))
        trimd-syn (-> synopsis
                      (string/replace #"\[|\]|\\|\n|,|=" "")
                      (string/replace #"\s+|\t+" " "))
        [out in] (string/split trimd-syn (re-pattern command))
        out (->> (-> out str string/trim
                     (string/split #" "))
                 (remove empty?)
                 vec)
        in  (->> (-> in str string/trim
                     (string/split #" "))
                 (remove empty?)
                 vec)]
    (swap! metadata-db assoc command {:out out :in in})))

