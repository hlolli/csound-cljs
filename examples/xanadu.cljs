(ns examples.xanadu
  (:require [csound.load :refer [load-csound!]]))

(load-csound! *ns*)

(defglobal sine-table (ftgen 0 0 8192 10 1))

(defglobal cosine-table (ftgen 0 0 8192 11 1))

(defglobal ln-table (ftgen 0 0 8192 -12 20))

(definstr instr-1 [freq]
  (let [shift 8/1200
        pch (cpspch freq)
        oct (octpch freq)
        vib (poscil:k 1/120 (/ pch 50) sine-table)
        ag (pluck 0.03 (cpsoct:k (+ oct vib)) 1000 sine-table 1)
        aleft (pluck 0.03 (cpsoct:i (+ oct shift)) 1000 sine-table 1)
        aright (pluck 0.03 (cpsoct:i (- oct shift)) 1000 sine-table 1)
        f1 (expon:k 0.1 p3 1)
        f2 (expon:k 1 p3 0.1)
        drw (delayrw 2 ag)
        tap1 (deltapi drw f1)
        tap2 (deltapi drw f2)
        d1 (deltap drw 2)
        d2 (deltap drw 1.1)]
    (outs (+ aleft tap1 d1)
          (+ aright tap2 d2))))

(definstr instr-2 [freq]
  (let [shift 8/1200
        pch (cpspch freq)
        oct (octpch freq)
        vib (poscil:k 1/120 (/ pch 50) sine-table)
        ag (pluck 0.03 (cpsoct:k (+ oct vib)) 1000 sine-table 1)
        aleft (pluck 0.03 (cpsoct:i (+ oct shift)) 1000 sine-table 1)
        aright (pluck 0.03 (cpsoct:i (- oct shift)) 1000 sine-table 1)
        drw (delayrw 2 ag)
        d1 (deltap drw 0.1)
        d2 (deltap drw 0.2)]
    (outs (+ aleft d1)
          (+ aright d2))))

(definstr instr-3 [freq mod-a mod-b]
  (let [shift 8/1200
        pch (cpspch freq)
        oct (octpch freq)
        third-time (/ p3 3)
        kadsr (linseg:k 0 third-time 1 third-time 1 third-time 0)
        kmodi (linseg:k 0 third-time 5 third-time 3 third-time 0)
        kmodr (linseg:k mod-a p3 mod-b)
        ksig1 (* kmodi (/ (- kmodr 1) kmodr) 2)
        asig1index (abs:a (upsamp (/ (* ksig1 2) 20)))
        ksig2 (* kmodi (+ kmodr 1) 0.5)
        asig3 (tablei:a asig1index ln-table)
        acos1 (poscil:a ksig1 pch cosine-table)
        asig4 (exp:a (+ (* -0.5 asig3) acos1))
        acos2 (poscil:a (* ksig2 pch) pch cosine-table)
        shiftl (cpsoct:i (- oct shift))
        shiftr (cpsoct:i (+ oct shift))
        aoutl (poscil:a (* 0.003 kadsr asig4) (+ acos2 shiftl))
        aoutr (poscil:a (* 0.003 kadsr asig4) (+ acos2 shiftr))]
    (outs aoutl aoutr)))



(map #(instr-3 0 15 % 2 0.2) [7.06 8.01 8.06 8.10 8.11 9.04])
(map #(instr-3 7.5 15 % 1.7 0.5) [6.02 6.09 7.02 7.06 6.11 7.04])
(map #(instr-3 15 15 % 1.4 0.58) [7.11 8.06 8.11 9.03 8.11 9.04])
(map #(instr-3 22.5 15 % 1.1 1.1) [6.09 7.04 8.09 8.01 7.11 8.04])
(map #(instr-3 30 15 % 0.8 1.4) [6.11 7.06 7.11 8.03 7.11 8.04])
(map #(instr-3 37.5 15 % 0.5 1.7) [5.07 6.02 6.07 6.11 6.11 7.04])
(map #(instr-3 45 15 % 0.2 2) [7.06 8.01 8.06 8.10 8.11 9.04])
(map-indexed #(instr-1 (* 0.1 %1) 10 %2) [8.06 9.01 9.06 9.10 9.11 10.04])
(map-indexed #(instr-2 (+ 7.5 (* 0.1 %1)) 10 %2) [8.02 8.09 9.02 9.06 9.11 10.04])
(map-indexed #(instr-2 (+ 15 (* 0.1 %1)) 10 %2) [8.11 9.06 9.11 10.03 9.11 10.04])
(map-indexed #(instr-2 (+ 22.5 (* 0.1 %1)) 10 %2) [8.09 9.04 10.09 10.01 9.11 10.04])
(map-indexed #(instr-2 (+ 30 (* 0.1 %1)) 10 %2) [8.11 9.06 9.11 10.03 9.11 10.04])
(map-indexed #(instr-2 (+ 37.5 (* 0.1 %1)) 10 %2) [8.07 9.02 9.07 9.11 9.11 10.04])
(map-indexed #(instr-1 (+ 45  (* 0.1 %1)) 10 %2) [9.06 10.01 10.06 10.10 10.11 11.04])

