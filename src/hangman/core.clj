(ns hangman.core
  (:require
    [clojure.string  :as str]
  ) )


(defn main []
  (println "Hello, World!")
  (let [ words   (->> (slurp"resources/words.txt")
                      (str/split-lines )
                      (map str/trim ) ) ]
    (println 
      (take 5 
        (map #(str \" % \") words) ))
  )
)
