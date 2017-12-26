(ns csound.load
  (:require [csound.connection]
            [csound.opcodes]
            [csound.patched-opcodes]
            [csound.operators])
  (:require-macros csound.macros))


(defn load-csound! [ns]
  (let [ns-name (.-name ns)
        hack-swap! (fn [k v]
                     (let [previous-data (get-in @cljs.env/*compiler*
                                                 [:cljs.analyzer/namespaces
                                                  ns-name k])]
                       (swap! cljs.env/*compiler*
                              assoc-in
                              [:cljs.analyzer/namespaces
                               ns-name k]
                              (if (empty? previous-data)
                                v
                                (into previous-data v)))))
        excludes '#{+ - / *}
        requires '{csound.core csound.core,
                   csound.opcodes csound.opcodes,
                   csound.patched-opcodes csound.patched-opcodes,
                   csound.operators csound.operators,
                   csound.connection csound.connection}
        require-macros '{csound.macros csound.macros}
        use-macros '{definstr csound.macros
                     defglobal csound.macros}
        all-opcodes (keys (get-in @cljs.env/*compiler*
                                  [:cljs.analyzer/namespaces
                                   'csound.opcodes :defs]))
        all-opcodes (reduce #(assoc %1 %2 'csound.opcodes) {} all-opcodes)
        all-operators (keys (get-in @cljs.env/*compiler*
                                    [:cljs.analyzer/namespaces
                                     'csound.operators :defs]))
        all-operators (reduce #(assoc %1 %2 'csound.operators) {} all-operators)
        all-patched-opcodes (keys (get-in @cljs.env/*compiler*
                                          [:cljs.analyzer/namespaces
                                           'csound.patched-opcodes :defs]))
        all-patched-opcodes (reduce #(assoc %1 %2 'csound.patched-opcodes) {} all-patched-opcodes)
        public-core-function '{write-csd! csound.core}]
    (hack-swap! :excludes excludes)
    (hack-swap! :requires requires)
    (hack-swap! :uses (merge all-opcodes all-operators all-patched-opcodes))
    (hack-swap! :require-macros require-macros)
    (hack-swap! :use-macros use-macros)
    :csound-symbols-loaded))
