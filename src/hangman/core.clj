(ns hangman.core
  (:require
    [clojure.string  :as str]
  ) )


(def show-info-size 10)

(defn show-info 
  "Print synopsis info about a sequence of strings"
  [ seqVals seqName ]
  (println 
    (str seqName "("  (count seqVals) ") " )
        (take show-info-size (map #(str \" % \") seqVals)) ))

(def all-words   
  "A collection of all words for the hangman game."
  (->> (slurp "resources/words.txt")
       (str/split-lines )
       (map str/trim ) ))

(def tst-words [ 
  "is" "at" "by" "up"
  "act" "add" "ace" "and" "ask" "abe" "ads"
  "bad" "bug" "bed" "bet"  "big" "bit"
  "cat" "car" "can" "cob" "con" "cop" "cup" 
  "work" "love" "hate" "jobs" "able" "ball" ] )

(defn max-word-length 
  "The maximum length of any word in a sequence."
  [words-map]
  { :pre  [ (map? words-map) ]
    :post [ (integer? %) ] }
  (apply max (keys words-map) ))

(defn words-by-length  
  "Maps word length to word values.  Each key is a word length. Each value is a collection
  of all words of that length.  Keys are absent if no words of that length are present."
  [word-seq]
  (group-by count word-seq) )

(defn to-word-array 
  "Returns all words of the specified length as a 2D array (vector of vectors).
  First index selects a given word, 2nd index selects chars from that word."
  [word-seq]
  { :pre  []
    :post [ (vector? %) ] }
  (vec
    (map vec word-seq) ))

(defn words-of-length 
  "Returns a vector of words of the specified length."
  [words-map length]
  { :pre  [ (integer? length) ]
    :post [ (vector? %) ] }
  (vec (get words-map length []) ))

(defn num-rows
  "Given a 2D array (vector of vectors), return the number of rows (1st dimension)."
  [word-array]
  { :pre  [ (vector? word-array) ]
    :post [] }
  (count word-array) )

(defn num-cols
  "Given a 2D array (vector of vectors), return the number of columns (2nd dimension)."
  [word-array]
  { :pre  [ (vector?  word-array) 
            (vector? (word-array 0)) ]
    :post [] }
  (count (word-array 0)) )

(defn get-array-col
  "Given a 2D array (vector of vectors), return a vector of elements 
  from the specified column."
  [word-array col-idx]
  { :pre  [ (integer? col-idx) ]
    :post [ (vector? %) ] }
  (reduce #(conj %1 (%2 col-idx))
          [] word-array ) )

(defn get-array-row
  "Given a 2D array (vector of vectors), return a vector of elements 
  from the specified row."
  [word-array row-idx]
  { :pre  [ (integer? row-idx) ]
    :post [ (vector? %) ] }
  (word-array row-idx) )

(defn freqs-by-col
  "For each column of a 2D array, computes an element frequency map. Returns as 
  a vector indexed by column."
  [word-array]
  { :pre  [ (vector? word-array)
            (< 0 (num-rows word-array) )
            (< 0 (num-cols word-array) ) ] 
    :post [ (vector? %) ] }
  (vec
    (for [idx (range 0 (inc (num-cols word-array) )) ]
      (frequencies (get-array-col idx)) )))


(defn run-tests []
  ; Manipulation of strings/vectors/character seq's
  (assert (= (vec "abcd")                [\a \b \c \d] ))
  (assert (= (str/join  (vec "abcd"))    "abcd" ))
  (assert (= (apply str (vec "abcd"))    "abcd" ))

  ; Test array slicing functions
  (let [ tstArr [ [:a :b :c] [1 2 3] [\a \b \c]] ]
    (assert (= (get-array-row tstArr 0) [:a :b :c] ))
    (assert (= (get-array-row tstArr 1) [ 1  2  3] ))
    (assert (= (get-array-row tstArr 2) [\a \b \c] ))
    (assert (= (get-array-col tstArr 0) [:a  1 \a] ))
    (assert (= (get-array-col tstArr 1) [:b  2 \b] ))
    (assert (= (get-array-col tstArr 2) [:c  3 \c] ))
  )

  ; Use of (frequencies...) and (merge-with...)
  (assert (= (frequencies "abbccc")          
             {\a 1, \b 2, \c 3} ))
  (assert (= (frequencies ["abc" "bc" "c"])  
             {"abc" 1 "bc" 1 "c" 1} ))
  (assert (= (map frequencies ["abc" "bc" "c"])
             [ {\a 1, \b 1, \c 1} {\b 1, \c 1} {\c 1} ] ))
  (assert (= (apply merge-with + (map frequencies ["abc" "bc" "c"]) )
             {\a 1 \b 2 \c 3} ))
)


(defn main 
  ( [] (main tst-words) )
  ( [word-seq]
      (let [words-map     (words-by-length word-seq)
            max-word-len  (apply max (keys words-map) )
            word-array    (to-word-array (words-map 2)) ]
        (run-tests)
        (show-info word-seq "ALL")
        (doseq [ curr-len (sort (keys words-map)) ]
          (show-info (words-of-length words-map curr-len) (str "len=" curr-len) ) ))
  )
)

(defonce sanity-check (run-tests) )

