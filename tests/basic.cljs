(ns tests.basic
  (:refer-clojure :exclude [+ -])
  (:require csound.core
            [csound.opcodes :as opc]
            [csound.patched-opcodes :as popc]
            [csound.operators :refer [+ -]]
            [cljs.test :refer-macros [deftest is testing run-tests
                                      use-fixtures]]
            csound.connection)
  (:require-macros [csound.macros :refer [definstr]]))

(defn nullify-gensym-counter []
  (set! csound.core/csnd_gensym_counter (atom 0)))

(defn to-string [expr]
  (csound.core/parse-to-string (expr)))

(definstr simple-oscil [amp freq]
  (opc/outc (apply + (map #(opc/poscil amp (+ % freq)) (range 0 1000 73)))))


(deftest test1
  (do (nullify-gensym-counter)
      (is (= "k1 poscil 1, 440\n" (to-string (opc/poscil:k 1 440))))))

(deftest test2
  (do (nullify-gensym-counter)
      (let [asig1 (csound.opcodes/poscil:a 1 200)
            asig2 (csound.opcodes/poscil:a asig1 300)
            buffer1 (popc/delayrw 2 asig2)
            atap1 (popc/deltap buffer1 1.1)
            atap2 (popc/deltap buffer1 1.1)
            atap3 (popc/deltap buffer1 1.1)
            aout (csound.opcodes/sum atap2 asig2 atap3)]
        (is (= (to-string aout)
               (str "a1 poscil 1, 200\na16 delayr 2\na10 deltap 1.1\na3 poscil a1, 300"
                    "\na8 deltap 1.1\ndelayw a3\na12 sum a8, a3, a10\n"))))))


;; (run-tests 'tests.basic)

