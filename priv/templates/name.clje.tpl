(ns {{name}})

(defn main [args]
  (println "Args:" (mapv str args))
  (erlang/halt 0))
