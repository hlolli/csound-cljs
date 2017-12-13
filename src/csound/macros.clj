(ns csound.macros)


(defmacro definstr
  [instr-name parameters body]
  `(def ~(vary-meta (symbol (name instr-name))
                    assoc :arglists (list (mapv symbol parameters)))
     (fn ~(mapv (comp gensym name) parameters)
       (~'binding [~'*global* false]
        (str "instr " ~(name instr-name) "\n"
             ('csound.core/parse-to-string
              (~body))
             "endin\n")))))

(comment
  
  (parse-to-string# [assembler#]
                    (letfn [(dedupe# [v#]
                              (reduce (fn [acc# node#]
                                        (if (some #(= (first node#) (first %)) acc#)
                                          acc#
                                          (conj acc# node#))) [] v#))
                            (reduce-to-string# [v#]
                              (reduce (fn [acc-str# [_# s#]]
                                        (str acc-str# s# "\n")) "" v#))]
                      (-> assembler# dedupe# reduce-to-string#)))
  
  
  (csound.macros$macros/definstr abc [d]
    1)

  (defn poscil [amp cps & [ifn]]
    (let [out-types-quoted 'AudioSignal
          out-types AudioSignal
          ast (ast-node out-types-quoted
                        "poscil"
                        [amp cps ifn]
                        false)]
      (new out-types ast))))


