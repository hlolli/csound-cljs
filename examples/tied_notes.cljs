(ns examples.tied-notes
  (:require [csound.load :refer [load-csound!]]))

(load-csound! *ns*)

(defglobal sine-table (ftgen 0 0 8192 10 1))

(definstr example-instr-1 [att freq]
  (let [dur (abs:i p3)
        phase (if:i (= (tival) 0) 0 -1)
        initial-env (init:a 0)
        env (line:a att dur 0) 
        slur (linseg:a 0 (* 0.3 dur) att (* 0.7 dur) 0)
        aenv (if:k tied?
                   (+= initial-env slur)
                   (mutate! initial-env env))
        asig (oscili:a aenv freq sine-table phase)]
    (outs asig asig)))


(do (example-instr-1 0   -5   0.8 451)
    (example-instr-1 1.5 -1.5 0.1 512)
    (example-instr-1 3   2    0.7 440))

