(ns hangman.log
  (:require
    [clojure.string  :as str]
  ))

; Simple logging tools for demo. Replace with log4j or similar in production.

(def ^:const NEVER  99 )
(def ^:const FATAL   6 )
(def ^:const ERROR   5 )
(def ^:const WARN    4 )
(def ^:const NORMAL  3 )
(def ^:const EXTRA   2 )
(def ^:const DEBUG   1 )
(def ^:const TRACE   0 )

(def logging-min-level  NORMAL )

(defn write-to-log
  "Write log msg to console for debugging."
  [level & msgs]
  (when (<= logging-min-level level)
    (apply println msgs ) ))

; Convenience functions
(defn error  [& msgs] (apply write-to-log  ERROR    msgs ))
(defn warn   [& msgs] (apply write-to-log  WARN     msgs ))
(defn msg    [& msgs] (apply write-to-log  NORMAL   msgs ))
(defn extra  [& msgs] (apply write-to-log  EXTRA    msgs ))
(defn dbg    [& msgs] (apply write-to-log  DEBUG    msgs ))


(defn set-min-level
  "Sets the minimum level for log messages to be reported."
  [level]
  (def logging-min-level level) )

