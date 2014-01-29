(ns hangman.core
  (:require
    [clojure.string     :as str]
  ) )


(defn main []
  (println "Hello, World!")
  (let [ wordFile  (slurp "resources/words.txt")
         ; _         (println "#1: " (take 25 wordFile))
         lines     (str/split-lines wordFile)
         ; _         (println "#2: " (take 5 lines))
         words     (map str/trim lines)
         ;_         (println "#3: " (take 5 (map #(str \" % \") words) ))
       ] 
    (println (take 5 (map #(str \" % \") words) ))
  )
)
