(ns csound.macros)


(defmacro defglobal [var-name csound-expression]
  `(def ~(symbol (name var-name))
     (~'binding [~'csound.core/*global* ~(name var-name)]
      (~'csound.core/global-tree-to-out ~csound-expression))))

(defmacro definstr
  [instr-name parameters body]
  `(def ~(vary-meta (symbol (name instr-name))
                    assoc :arglists (list (into ['start 'dur] parameters)))
     (let [csnd-instr-name# (~'csound.core/cljs->csound-instr-name
                             ~(name instr-name))
           orc-str# (str "instr " csnd-instr-name# "\n"
                         (~'binding [~'csound.core/*global* false
                                     ~'csound.core/*instr-name* csnd-instr-name#]
                          (~'csound.core/parse-to-string
                           (let ~(reduce (fn [i# p#]
                                           (conj i# (symbol p#)
                                                 (list 'new 'csound.core/ScoreParameter (+ 4 (/ (count i#) 2)))))
                                         [] parameters)
                             (~body))))
                         "endin\n")
           ;; params# ~(mapv (comp gensym name) parameters)
           ]
       (swap! ~'csound.core/transpiled-csound-data
              assoc-in [csnd-instr-name# :code] orc-str#)
       (println orc-str#)
       (if (:connection ~'@csound.connection/connection)
         ((:compile-orc-fn ~'@csound.connection/connection) orc-str#)
         orc-str#)
       (fn [start# dur# ~@parameters]
         (let [sco-str# (str "i \"" csnd-instr-name# "\" " start# " " dur# " "
                             (apply str (interpose " " ~parameters)))]
           (if (:connection ~'@csound.connection/connection)
             ((:input-message-fn ~'@csound.connection/connection) sco-str#)
             (binding [*print-fn* *print-err-fn*]
               (println "Error: no Csound connection!"))))))))




