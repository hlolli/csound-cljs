(ns csound.connection)

(def connection (atom nil))

(def browser?
  (exists? js/window))

(when-not browser?
  (let [dgram (js/require "dgram")
        socket-client (.createSocket dgram "udp4")
        spawn (.-spawn (js/require "child_process"))
        compile-orc-fn (fn [orc-str]
                         (.send socket-client orc-str 6006 "localhost"
                                (fn [err]
                                  (when err
                                    (binding [*print-fn* *print-err-fn*]
                                      (println "Error in csound socket: " err))))))
        input-message-fn (fn [score-str]
                           (.send socket-client (str "$" score-str) 6006 "localhost"
                                  (fn [err]
                                    (when err
                                      (binding [*print-fn* *print-err-fn*]
                                        (println "Error in csound socket: " err))))))]
    (when (nil? @connection)
      (let [csound-udp-connection (spawn "csound" #js ["-odac" "--port=6006" "--0dbfs=1"])]
        (.on csound-udp-connection "close"
             #(println "Csound UDP connection ended"))
        #_(.on (.-stdout csound-udp-connection) "data"
               #(println (.toString %)))
        #_(.on (.-stderr csound-udp-connection) "data"
               #(println (.toString %)))
        (reset! connection {:connection csound-udp-connection
                            :compile-orc-fn compile-orc-fn
                            :input-message-fn input-message-fn})
        (run! (fn [event-type]
                (.on js/process event-type
                     (fn [] (do (.kill csound-udp-connection)
                                (js/setTimeout #(.exit js/process 0) 500)))))
              ["SIGINT" "SIGTERM" "SIGHUP" "exit"])))))

