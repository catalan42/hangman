(ns hangman.core
  (:require
    [clojure.string  :as str]
    [clojure.set     :as set]
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

(def ^:const all-letters (map char (range (int \a) (inc(int \z)) )))

(def tst-words [ 
  "is" "at" "by" "up"
  "act" "add" "ace" "and" "ask" "abe" "ads"
  "bad" "bug" "bed" "bet"  "big" "bit"
  "cat" "car" "can" "cob" "con" "cop" "cup" 
  "work" "love" "hate" "jobs" "able" "ball" ] )

(defn to-words-by-length  
  "Maps word length to word values.  Each key is a word length. Each value is a collection
  of all words of that length.  Keys are absent if no words of that length are present."
  [word-seq]
  (group-by count word-seq) )

(defn to-word-array 
  "Returns all words of the specified length as a 2D array (vector of vectors).
  First index selects a given word, 2nd index selects chars from that word."
  [word-seq]
  (vec
    (map vec word-seq) ))

(defn num-rows
  "Given a 2D array (vector of vectors), return the number of rows (1st dimension)."
  [word-array]
  (count word-array) )

(defn num-cols
  "Given a 2D array (vector of vectors), return the number of columns (2nd dimension)."
  [word-array]
  (count (word-array 0)) )

(defn get-array-col
  "Given a 2D array (vector of vectors), return a vector of elements 
  from the specified column."
  [word-array col-idx]
  { :pre  [ (vector? word-array) (integer? col-idx) ]
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

(defn to-freqs-by-col
  "For each column of a 2D array, computes an element frequency map. Returns as 
  a vector indexed by column."
  [word-array]
  { :pre  [ (vector? word-array)
            (< 0 (num-rows word-array) )
            (< 0 (num-cols word-array) ) ] 
    :post [ (vector? %) ] }
  (vec
    (for [idx (range 0 (num-cols word-array)) ]
      (let [curr-col (get-array-col word-array idx)]
        (frequencies curr-col)) )))

(defn filter-with-idx
  "Returns values from data-seq where corresponding pred-seq elements are truthy.
  An indexed-based implementation."
  [pred-seq data-seq]
  { :pre  [ (= (count pred-seq) (count data-seq)) ] 
    :post [ (vector? %) ] }
  (let [data-vec  (vec data-seq)
        pred-vec  (vec pred-seq)
        filt-fn   (fn [idx data-val] 
                    (if (pred-vec idx) (data-vec idx) ))
        filt-seq  (keep-indexed filt-fn data-vec) 
  ] (vec filt-seq) ))

(defn filter-with-seq
  "Returns values from data-seq where corresponding pred-seq elements are truthy.
  A sequence-based implementation"
  [pred-seq data-seq]
  (let [pred-data-pairs   (map vector pred-seq data-seq )
        filt-seq          (filter #(first %) pred-data-pairs)
        filt-data         (map second filt-seq) 
  ] (vec filt-data) ))

(defn filter-with 
  "Returns values from data-seq where corresponding pred-seq elements are truthy."
  [pred-seq data-seq]
  { :pre  [ (= (count pred-seq) (count data-seq)) ] 
    :post [ (vector? %) ] }
  (filter-with-seq pred-seq data-seq) )


(defn guess-matches?
  "Returns true if a guess matches the target word. The target word is a vector of
  characters.  The guess value is a vector of the same length with elements that are
  either a character or nil, where nil indicates a wildcard that matches any character in
  the target word."
  [word guess]
  { :pre  [ (= (count word) (count guess)) 
            (not-any? nil? word) ]
    :post [] }
  (let [ pair-seq   (map vector guess word)
         result     (every? #(or (=    (first %) (second %))
                                 (nil? (first %)) )
                      pair-seq) ]
    result ))

(defn word-has-letter?
  "Returns true if a letter appears in a word, else nil."
  [word letter]
  (some #(= % letter) word) )

(def ^:const log-2 (Math/log 2) )

(defn log-base-2
  "Calculates the base-2 logarithm."
  [val]
  { :pre  [ (< 0 val) ]
    :post [] }
  (/ (Math/log val) log-2) )

(defn calc-entropy
  "Calculates the entropy of a binary probability value."
  [prob]
  { :pre  [ (<= 0 prob 1) ]
    :post [ (<= 0 % ) ] }
  (let [prob-orig (double prob)]
    (cond
      (= 0.0 prob-orig) 0  ; avoid trying to compute log(0)
      (= 1.0 prob-orig) 0  ; avoid trying to compute log(0)
      :normal 
        (let [prob-comp (- 1.0 prob-orig)]  ; complementary prob-orig
          (+ (- (* prob-orig (log-base-2 prob-orig     ) ))
             (- (* prob-comp (log-base-2 prob-comp) )) )) )))

(defn calc-info-bits
  "Calculates the number of bits of information gained for the guess-letter."
  [words guess-letter]
  (let [total-words     (count words)
        match-words     (count (filter true? 
                          (map #(word-has-letter? % guess-letter) words) ))
        ratio           (/ (double match-words) (double total-words))
        bits            (calc-entropy ratio) ]
    bits ))

(defn make-guess
  "Generate the next guess letter by calculating the bits of information for each possible
  guess letter. "
  [keep-words used-chars]
  (let [
        avail-chars   (set/difference (set all-letters) used-chars)
        char-bits     (zipmap avail-chars (map #(calc-info-bits keep-words %) avail-chars) )
        best-char     (apply max-key char-bits avail-chars ) ]
    best-char ) )

(defn make-guess-freq
  "Generate the next guess letter, based on letter frequencies in the surviving words. "
  [keep-words used-chars]
  (let [
    col-char-freqs  (to-freqs-by-col keep-words)
    all-char-freqs    (apply merge-with + col-char-freqs)
    avail-chars       (set/difference (set(keys all-char-freqs)) used-chars)
      ; _ (println "avail-chars" avail-chars )
    max-avail-char    (apply max-key all-char-freqs avail-chars ) 
  ] max-avail-char ) )

(defn make-clue
  "Generate a clue given the target word and a vec of guessed letters."
  [target-vec guesses-set]
  (vec 
    (for [ letter target-vec ] (guesses-set letter)) ))

(defn do-tests []
  ; Filtering one sequence with another
  (let [pred-vals5  [ true false 5 nil :a ] 
        pred-vals8  [ true false true nil true nil false true ] 
  ]
    (assert (= (filter-with     pred-vals5 (range 5)) [0 2 4  ] ))
    (assert (= (filter-with     pred-vals8 (range 8)) [0 2 4 7] )) 
    (assert (= (filter-with-seq pred-vals8 (range 5)) [0 2 4  ] ))
    (assert (= (filter-with-seq pred-vals8 (range 8)) [0 2 4 7] )) 
  )

  ; Manipulation of strings/vectors/character seq's
  (assert (= (vec "abcd")                [\a \b \c \d] ))
  (assert (= (str/join  (vec "abcd"))    "abcd" ))
  (assert (= (apply str (vec "abcd"))    "abcd" ))

  ; Test array slicing functions
  (let [tstArr [ [:a :b :c] [1 2 3] [\a \b \c]] ]
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

  ; Match guesses
  (assert (guess-matches? "abcd" "abcd") )
  (assert (guess-matches? "abcd" [\a \b \c \d]) )
  (assert (guess-matches? "abcd" [nil nil nil nil]) )
  (assert (guess-matches? "abcd" [\a nil nil nil]) )

  (let [
    tst-words [ "abcd" "xbcd" "xxcd" "xxxd" ] 
    words-map           (to-words-by-length tst-words)
      _ (assert (= words-map   {4 ["abcd" "xbcd" "xxcd" "xxxd"]} ))

    curr-len            4
    curr-words          (words-map curr-len)
      _ (assert (= curr-words     ["abcd" "xbcd" "xxcd" "xxxd"]  ))
    word-array          (to-word-array  curr-words)
      _ (assert (= word-array [[\a \b \c \d] [\x \b \c \d] [\x \x \c \d] [\x \x \x \d]] ))

    col-char-freqs      (to-freqs-by-col word-array)
      _ (assert (= col-char-freqs [{\a 1, \x 3} {\b 2, \x 2} {\c 3, \x 1} {\d 4}] ))
    all-char-freqs      (apply merge-with + col-char-freqs)
      _ (assert (= all-char-freqs { \a 1, \b 2, \c 3, \d 4, \x 6} )) 
    max-freq-val        (apply max-key all-char-freqs (keys all-char-freqs) )
      _ (assert (= max-freq-val \x ))

    tgt-word            [ \x  \b  \c  \d  ]
    clue                [ nil \b  nil nil ]
    keep-flag           (map #(guess-matches? % clue ) word-array )
      _ (assert (= keep-flag [true true false false]))
    keep-words          (filter-with keep-flag curr-words)
      _ (assert (= keep-words ["abcd" "xbcd"] ))
    guessed-chars       #{ \b }
      _ (assert (= guessed-chars #{\b} ))
    new-guess           (make-guess-freq word-array guessed-chars)
      _ (assert (= new-guess \x ))
    guessed-chars       (conj guessed-chars new-guess)
      _ (assert (= guessed-chars #{\b \x} ))
    new-clue            (make-clue tgt-word guessed-chars)
      _ (assert (= new-clue [\x \b nil nil] ))
  ] )
)

(defn format-clue
  "Format the clue vector into a nice-looking string."
  [clue]
  (str/join 
    (map  #(if (nil? %) "-" % )  clue) ))

(comment
(defn filter-words
  "Calculates which words are possible matches given the guessed letters and clue string."
  [curr-words clue guessed-chars]
  (let [keep-chars      (for [clue-char clue :when (not(nil? clue-char))] clue-char)
        fail-chars      (set/difference guessed-chars keep-chars)
        keep-flag       (map #(guess-matches? % clue ) word-array )
        fail-flag       (map #(guess-matches? % clue ) word-array )
    keep-words      (filter-with keep-flag word-array) ] )
)
)

(defn main 
  ( [] 
    (main all-words) )
  ( [word-seq]
    (do-tests)
    (println "----------------------------------------")
    (def tgt-word         "uniformed")
    (def words-map        (to-words-by-length word-seq) )
    (def curr-len         (count tgt-word) )
    (def curr-words       (words-map curr-len) )
    (def word-array       (to-word-array  curr-words) )
    (def max-word-len     (apply max (keys words-map)) )

    (println)
    (println "************************************************************")
    (println "word-array" (take 20 (map str/join word-array)) )
    (loop [ guessed-chars  #{}
            clue           (make-clue tgt-word guessed-chars) 
          ]
      (println )
      (let [
        keep-flag       (map #(guess-matches? % clue ) word-array )
        keep-words      (filter-with keep-flag word-array) ]
          (println "clue: " (format-clue clue) 
                    "  keep-words(" (count keep-words) "):" 
                    (take 10 (map str/join keep-words)) "..." )
          (if (= 1 (num-rows keep-words))
            (let [final-guess (str/join (keep-words 0)) ]
              (println (str "***** found word:  '" final-guess "'  *****") )
              (println "matches:" (= final-guess tgt-word)) )
          ;else
            (let [
              new-guess       (make-guess keep-words guessed-chars)
              guessed-chars   (conj guessed-chars new-guess)
              new-clue        (make-clue tgt-word guessed-chars)
                _ (println "  new-guess" new-guess 
                   "  guessed-chars (" (count guessed-chars) ")" guessed-chars)
              ]
              (if (some nil? new-clue)
                (recur  (conj guessed-chars new-guess)  new-clue ) 
              ))
          )
      ))

  )
)
(defn -main [& args] (apply main args) )

; (defonce sanity-check (do-tests) )
