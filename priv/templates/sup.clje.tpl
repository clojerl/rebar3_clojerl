(ns {{name}}.sup)

(def sup-flags #erl{:strategy  :one_for_one
                    :intensity 1
                    :period    5})

(def child-specs #erl())

(defn start-link []
  (supervisor/start_link #erl[:local :try-clojerl.sup]
                         :try-clojerl.sup
                         #erl()))

(defn init [_]
  #erl[:ok #erl[sup-flags child-specs]])
