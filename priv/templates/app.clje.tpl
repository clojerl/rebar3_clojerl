(ns {{name}}.app
  (:require [{{name}}.sup :as sup]))

(defn start [type args]
  (sup/start-link))

(defn stop [state]
  state)
