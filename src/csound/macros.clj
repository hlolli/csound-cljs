(ns csound.macros
  (:refer [csound.core :refer [parse-to-string]]))

(defmacro definst
  [instr-name parameters body]
  `(defn ~(symbol (name instr-name)) parameters
     (parse-to-string ~(body))))
