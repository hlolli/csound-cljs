(ns scripts.metadata
  (:require [fs]
            [path]
            [xml2json :as xml]
            [clojure.string :as string]
            [clojure.zip :as zip]))

(def manual-opcodes-dir
  "/home/hlolli/csound/manual/opcodes/")

(def fl-opcodes (fn [file] (re-find #"^fl[^a]" file)))

(def python-opcodes (fn [file] (re-find #"^py" file)))

(def spectral-opcodes #{"spectrum.xml" "specsum.xml"
                        "specscal.xml" "specptrk.xml" "spechist.xml"
                        "specfilt.xml" "specdisp.xml" "specdiff.xml"
                        "specaddm.xml"})

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
                  "kdump4.xml" "kexprand.xml" "xyin.xml"
                  "push_f.xml" "push.xml" "pop.xml" "pop_f.xml"
                  "soundout.xml" "soundouts.xml" "stack.xml"
                  "GEN22.xml" "vbap16.xml" "vbap16move.xml"
                  "vbap8.xml" "vbap8move.xml" "vbap4.xml" "vbap4move.xml"
                  "sliderkawai.xml" "lorisread.xml" "lorisplay.xml"
                  "lorismorph.xml" "ktableseg.xml" "changed2.xml"
                  "sndload.xml" "bformenc.xml" "hdf5write.xml"
                  "hdf5read.xml" "bformdec.xml" "hrtfer.xml"
                  "nchnls_i.xml" "p5gdata.xml" "p5gconnect.xml"
                  "array.xml"})

;; note opnot.xml is logial not(!)
(def operators #{"abs.xml" "modulus.xml"
                 "greaterthan.xml" "greaterequal.xml"
                 "lessthan.xml" "lessequal.xml"
                 "multiplies.xml" "adds.xml"
                 "subtracts.xml" "divides.xml"
                 "assign.xml" "plusbecomes.xml"
                 "frac.xml" "int.xml"
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
                 "kweibull.xml" "peakk.xml" "pchoct.xml"
                 "qnan.xml" "min.xml" "opnot.xml" "notequal.xml"
                 "ceil.xml" "max.xml" "until.xml" "tb.xml"
                 "cngoto.xml" "while.xml" "opand.xml" "dbamp.xml"
                 "cpsmidinn.xml" "ampdbfs.xml" "taninv2.xml" "urd.xml"
                 "ftlen.xml" "cpsoct.xml" "nsamp.xml" "pchmidinn.xml"
                 "ops.xml" "cpspch.xml" "ftcps.xml" "ftlptim.xml"
                 "ftsr.xml" "opa.xml" "opk.xml" "logbtwo.xml" "divz.xml" "octave.xml"
                 "qinf.xml" "ftchnls.xml" "rnd.xml" "db.xml" "powoftwo.xml"
                 "cent.xml" "semitone.xml" "octmidinn.xml" "octcps.xml" "octpch.xml"
                 "dbfsamp.xml" "birnd.xml" "ampdb.xml" "signum.xml"
                 "reinit.xml" "pow.xml" "loop_gt.xml" "loop_le.xml"
                 "loop_ge.xml" "rigoto.xml" "loop_lt.xml"})

(def redundants #{"convle.xml" "fiopen.xml" "oscilx.xml" "sense.xml"})

(def native-deps #{"LinkBeatForce.xml" "LinkBeatGet.xml" "LinkBeatRequest.xml" "LinkCreate.xml"
                   "LinkEnable.xml" "LinkIsEnabled.xml" "LinkMetro.xml" "LinkPeers.xml"
                   "LinkTempoGet.xml" "LinkTempoSet.xml" "faustaudio.xml" "faustcompile.xml"
                   "faustctl.xml" "faustgen.xml" "websocket.xml" "vstaudio.xml" "vstbankload.xml"
                   "vstedit.xml" "vstinfo.xml" "vstinit.xml" "vstmidiout.xml" "vstnote.xml"
                   "vstparamset.xml" "vstprogset.xml" "JackoAudioInConnect.xml" "JackoAudioIn.xml"
                   "JackoAudioOutConnect.xml" "JackoAudioOut.xml" "JackoFreewheel.xml" "JackoInfo.xml"
                   "JackoInit.xml" "JackoMidiInConnect.xml" "JackoMidiOutConnect.xml" "JackoMidiOut.xml"
                   "JackoNoteOut.xml" "JackoOn.xml" "JackoTransport.xml" "STKBandedWG.xml" "STKBeeThree.xml"
                   "STKBlowBotl.xml" "STKBlowHole.xml" "STKBowed.xml" "STKBrass.xml" "STKClarinet.xml"
                   "STKDrummer.xml" "STKFlute.xml" "STKFMVoices.xml" "STKHevyMetl.xml" "STKMandolin.xml"
                   "STKModalBar.xml" "STKMoog.xml" "STKPercFlut.xml" "STKPlucked.xml" "STKResonate.xml"
                   "STKRhodey.xml" "STKSaxofony.xml" "STKShakers.xml" "STKSimple.xml" "STKSitar.xml"
                   "STKStifKarp.xml" "STKTubeBell.xml" "STKVoicForm.xml" "STKWhistle.xml" "STKWurley.xml"
                   "wiiconnect.xml" "wiidata.xml" "wiirange.xml" "wiisend.xml"
                   "lua_exec.xml" "lua_opcall.xml" "lua_opdef.xml" "directory.xml"
                   "system.xml" "cudanal.xml" "cudasliding.xml" "cudasynth.xml"
                   "socksend.xml" "sockrecv.xml"})

(def csound-macro #{"ifndef.xml" "dollar.xml" "undef.xml" "define.xml"
                    "ifdef.xml" "include.xml"})

(def edge-cases #{"subinstr.xml" "subinstrinit.xml" "template.xml"
                  "xin.xml" "xout.xml"})


(def manual-opcode-files
  (->> (fs/readdirSync manual-opcodes-dir)
       js->clj
       (remove #(re-find #"^[i]?slider*" %))
       (remove #(= "0dbfs.xml" %))
       (remove #(= "sr.xml" %))
       (remove #(= "kr.xml" %))
       (remove #(= "ksmps.xml" %))
       (remove #(= "nchnls.xml" %))
       (remove #(= "p.xml" %))
       (remove #(= "splitrig.txt" %))
       (remove #(= "A4.xml" %))
       (remove #(= "top.xml" %))
       (remove #(= "topXO.xml" %))
       (remove deprecated)
       (remove operators)
       (remove redundants)
       (remove fl-opcodes)
       (remove python-opcodes)
       (remove native-deps)
       (remove csound-macro)
       (remove edge-cases)
       (remove spectral-opcodes)))

(def metadata-db (atom {}))

(defn lookup-key [k coll]
  (let [coll-zip (zip/zipper coll? #(if (map? %) (vals %) %) nil coll)]
    (loop [x coll-zip]
      (when-not (zip/end? x)
        (if-let [v (-> x zip/node k)] v (recur (zip/next x)))))))

(defn quote-mark-to-S [s]
  (if-let [first-q (string/index-of s "quot;")]
    (let [s (string/replace s "&" "")
          second-q (string/index-of s "quot;" (inc first-q))
          second-q-end (+ 5 second-q)
          third-q (string/index-of s "quot;" (inc second-q))
          part1 (subs s 0 first-q)
          part2 (subs s first-q second-q-end)
          part3 (subs s second-q-end (count s))
          _ [part1 part2 part3]
          out-string (str part1
                          "S"
                          (-> part2
                              (string/replace "quot;" "")
                              (string/replace " " "-"))
                          part3)]
      (if third-q
        (quote-mark-to-S out-string)
        out-string))
    s))

(doseq [file manual-opcode-files
        ;; file ["ftload.xml"]
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
                      quote-mark-to-S
                      (string/replace #"\[|\]|\\|\n|,|=|\." " ")
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


(defn patch! [opcode out in]
  (swap! metadata-db assoc opcode {:out out :in in}))

(do
  (swap! metadata-db dissoc "printf</command> and <command>printf_i")
  (patch! "tableshuffle" [] ["kitablenum"])
  (patch! "tableshufflei" [] ["itablenum"])
  (patch! "tab_i" ["ires"] ["iindex" "ifn" "imode"])
  (patch! "zdf_2pole_mode" ["alp" "abp" "ahp"] ["ain" "xcf" "xQ" "istor"])
  (patch! "zamod" ["ares"] ["asig" "kzamod"])
  (patch! "zkmod" ["kres"] ["ksig" "kzkmod"])
  (patch! "xyscale" ["kout"] ["kx" "ky" "kZeroZero" "kOneZero" "kZeroOne" "kOneOne"])
  (patch! "tempo" [] ["ktempo" "istarttempo"])
  (patch! "product" ["ares"] ["asignalorarray" "asig"])
  (patch! "strcpy" ["Sdestination"] ["Ssource"])
  (patch! "sprintf" ["Sresult"] ["Sformatstring" "xarg"])
  (patch! "printf" ["(null)"] ["Sformatstring" "karg"])
  (patch! "printf_i" ["(null)"] ["Sformatstring" "iarg"])
  ;; (patch! "sockrecv" ["asig"] ["iport" "ilength"])
  (patch! "delayk" ["kout"] ["ksig" "idelay" "imode"])
  (patch! "vdel_k" ["kout"] ["ksig" "kdelay" "imaxdelay" "imode"])
  (patch! "chn_k" ["Sname"] ["Sname" "imode" "itype" "idflt" "imin"
                             "ima" "ix" "iy" "iwidth" "iheight" "Sattribute"])
  (patch! "chn_a" ["Sname"] ["Sname" "imode" "itype" "idflt" "imin"
                             "ima" "ix" "iy" "iwidth" "iheight" "Sattribute"])
  (patch! "chn_S" ["Sname"] ["Sname" "imode" "itype" "idflt" "imin"
                             "ima" "ix" "iy" "iwidth" "iheight" "Sattribute"])
  (patch! "harmon2" ["ares"] ["asig" "koct" "kfrq1" "kfrq2" "icpsmode" "ilowest" "ipolarity"])
  (patch! "harmon3" ["ares"] ["asig" "koct" "kfrq1" "kfrq2" "kfrq3" "icpsmode" "ilowest" "ipolarity"])
  (patch! "harmon4" ["ares"] ["asig" "koct" "kfrq1" "kfrq2" "kfrq3" "kfrq4" "icpsmode" "ilowest" "ipolarity"])
  (patch! "taninv" ["ires"] ["iradian"])
  (patch! "taninv2" ["ires"] ["iradian"])
  nil)
