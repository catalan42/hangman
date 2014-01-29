(ns hangman.core
  (:require
    [clojure.string     :as str]
  ) )


(defn main []
  (println "Hello, World!")
  (let [ lines (str/split-lines 
                 (slurp"resources/words.txt") )
         words  (map str/trim lines )
       ] 
    (println (take 5 (map #(str \" % \") words) ))
  )
)
