(ns csound.macros)

(defmacro definstr
  [instr-name parameters body]
  `(def ~(vary-meta (symbol (name instr-name))
                    assoc :arglists (list (into ['start 'dur] (mapv symbol parameters))))
     (~'binding [~'csound.core/*global* false]
      (let [csnd-instr-name# (~'csound.core/cljs->csound-instr-name
                              ~(name instr-name))
            orc-str# (str "instr " csnd-instr-name# "\n"
                          (~'csound.core/parse-to-string
                           (~body))
                          "endin\n")
            params# ~(mapv (comp gensym name) parameters)]
        (if (:connection ~'@csound.connection/connection)
          ((:compile-orc-fn ~'@csound.connection/connection) orc-str#)
          orc-str#)
        (fn [start# dur# ~@(map (comp gensym name) parameters)]
          (if (:connection ~'@csound.connection/connection)
            ((:input-message-fn ~'@csound.connection/connection)
             (str "i \"" csnd-instr-name# "\" " start# " " dur# " "
                  (apply str (interpose " " params#))))
            (binding [*print-fn* *print-err-fn*]
              (println "Error: no Csound connection!"))))))))


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


