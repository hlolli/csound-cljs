(ns csound.patched-opcodes
  (:require [cljs.spec.alpha :as s]
            [cljs.spec.test.alpha :as stest]
            [csound.opcodes]
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
                                 valid-iArr? valid-iArr?*]]))

(defn delayrw-instance? [v]
  (= :delayrw (:patch v)))

(defn delayrw
  {:arglists '([i-delay-time audio-signal-to-write])}
  [i-delay-time audio-signal-to-write]
  (let [out-types-quoted 'IO
        out-types IO
        ast (ast-node out-types-quoted
                      nil
                      [i-delay-time audio-signal-to-write]
                      *global*
                      nil
                      :delayrw)]
    (new out-types ast)))

(s/fdef delayrw
  :args (s/cat :i-delay-time valid-i? :audio-signal-to-write valid-ar?))
(stest/instrument `deltayrw)

(defn deltap
  {:arglists '([delayrw-instance k-delay-time])}
  [delayrw-instance k-delay-time]
  (let [out-types-quoted 'AudioSignal
        out-types AudioSignal
        ast (ast-node out-types-quoted
                      "deltap"
                      [k-delay-time]
                      *global*
                      nil
                      delayrw-instance)]
    (new out-types ast)))

(s/fdef deltap
  :args (s/cat :delayrw-instance delayrw-instance?
               :k-delay-time valid-kr?))
(stest/instrument `deltap)

(defn deltapi
  {:arglists '([delayrw-instance x-delay-time])}
  [delayrw-instance x-delay-time]
  (let [out-types-quoted 'AudioSignal
        out-types AudioSignal
        ast (ast-node out-types-quoted
                      "deltapi"
                      [x-delay-time]
                      *global*
                      nil
                      delayrw-instance)]
    (new out-types ast)))

(s/fdef deltapi
  :args (s/cat :delayrw-instance delayrw-instance?
               :x-delay-time valid-x?))
(stest/instrument `deltapi)

(defn deltapxw
  {:arglists '([delayrw-instance ain adel iwsize & iwsize*])}
  [delayrw-instance in del wsize & [ wsize* ]]
  (let [out-types-quoted 'IO
        out-types IO
        ast (ast-node out-types-quoted
                      "deltapxw"
                      [in del wsize wsize*]
                      *global*
                      nil
                      delayrw-instance)]
    (new out-types ast)))

(s/fdef deltapxw
  :args (s/cat :delayrw-instance delayrw-instance?
               :in valid-ar?
               :del valid-ar?
               :wsize valid-i?
               :wsize* (s/? valid-i?*)))
(stest/instrument `deltapxw)

(defn deltapx
  {:arglists '([delayrw-instance adel iwsize & iwsize*])}
  [delayrw-instance del wsize & [ wsize* ]]
  (let [out-types-quoted 'AudioSignal
        out-types AudioSignal
        ast (ast-node out-types-quoted
                      "deltapx"
                      [del wsize wsize*]
                      *global*
                      nil
                      delayrw-instance)]
    (new out-types ast)))

(s/fdef deltapx
  :args (s/cat :delayrw-instance delayrw-instance?
               :del valid-ar?
               :wsize valid-i?
               :wsize* (s/? valid-i?*)))
(stest/instrument `deltapx)

(defn deltapn
  {:arglists '([delayrw-instance xnumsamps & inumsamps*])}
  [delayrw-instance numsamps & [ numsamps* ]]
  (let [out-types-quoted 'AudioSignal
        out-types AudioSignal
        ast (ast-node out-types-quoted
                      "deltapn"
                      [numsamps numsamps*]
                      *global*
                      nil
                      delayrw-instance)]
    (new out-types ast)))

(s/fdef deltapn
  :args (s/cat :delayrw-instance delayrw-instance?
               :numsamps valid-x?
               :numsamps* (s/? valid-i?*)))
(stest/instrument `deltapn)

(defn deltap3
  {:arglists '([delayrw-instance xdlt & idlt*])}
  [delayrw-instance dlt & [ dlt* ]]
  (let [out-types-quoted 'AudioSignal
        out-types AudioSignal
        ast (ast-node out-types-quoted
                      "deltap3"
                      [dlt dlt*]
                      *global*
                      nil
                      delayrw-instance)]
    (new out-types ast)))

(s/fdef deltap3
  :args (s/cat :delayrw-instance delayrw-instance?
               :dlt valid-x?
               :dlt* (s/? valid-i?*)))
(stest/instrument `deltap3)
