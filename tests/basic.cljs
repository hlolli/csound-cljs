(ns tests.basic
  (:refer-clojure :exclude [+ -])
  (:require csound.core
            [csound.opcodes :as opc]
            [csound.operators :refer [+ -]]
            [cljs.test :refer-macros [deftest is testing run-tests
                                      use-fixtures]]
            csound.connection)
  (:require-macros [csound.macros :refer [definstr]]))

(defn to-string [expr]
  (csound.core/parse-to-string (expr)))

(definstr simple-oscil []
  (let [k1 (opc/poscil:k 100 5.5)
        a1 (opc/poscil:a (+ k1 400) 5.5)]
    (opc/out a1)))

((:input-message-fn @csound.connection/connection) "i simple_oscil 1 0 1")

(keys (get-in @cljs.env/*compiler*
              [:cljs.analyzer/namespaces
               'tests.basic
               :excludes]))

(println (simple-oscil))


(apply + [(opc/poscil:a 400 5.5)
          (opc/poscil:a 400 5.5)])

(number? 1)

(deftest test1
  (is (re-find #"gk[0-9]+ poscil 1, 440\n" (to-string (opc/poscil:k 1 440)))))

(deftest test2
  )






