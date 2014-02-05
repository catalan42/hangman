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

(def ^:const all-letters (set (map char (range (int \a) (inc(int \z)) ))) )

(def tst-words [ 
  "is" "at" "by" "up"
  "act" "add" "ace" "and" "ask" "abe" "ads"
  "bad" "bug" "bed" "bet"  "big" "bit"
  "cat" "car" "can" "cob" "con" "cop" "cup" 
  "work" "love" "hate" "jobs" "able" "ball" ] )

(defn guess-matches?
  "Returns true if a guess matches the target word (a string). The guess value is a vector
  of the same length with elements that are either a character or nil, where nil indicates
  a wildcard that matches any character in the target word."
  [word guess]
  { :pre  [ (= (count word) (count guess)) 
            (not-any? nil? word) ]
    :post [] }
  (let [ pair-seq   (map vector guess word)  ; a sequence of pairs
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
  (let [prob-orig (double prob)
        prob-comp (- 1.0 prob-orig) ]  ; complement of prob-orig
    (cond
      (= 0.0 prob-orig) 0  ; avoid trying to compute log(0)
      (= 1.0 prob-orig) 0  ; avoid trying to compute log(0)
      :normal 
          (+ (- (* prob-orig (log-base-2 prob-orig  )))
             (- (* prob-comp (log-base-2 prob-comp  ))) ) )))

(defn calc-info-bits
  "Calculates the number of bits of information gained for the guess-letter."
  [words guess-letter]
  (let [total-words     (count words)
        match-words     (count (filter #(word-has-letter? % guess-letter) words) )
        word-ratio      (/ (double match-words) (double total-words)) ]
    (calc-entropy word-ratio) ))

(defn make-guess
  "Generate the next guess letter by calculating the bits of information for each possible
  guess letter. "
  [words used-chars]
  (let [avail-chars   (set/difference all-letters used-chars)
        char-bits     (zipmap avail-chars (map #(calc-info-bits words %) avail-chars) )
        best-char     (apply max-key char-bits avail-chars) ]
    best-char ) )

(defn make-clue
  "Generate a clue given the target word and a vec of guessed letters. The clue is a
  vector the length of the target word consisting of either letters (for correctly guessed
  letters) or nil (for incorrect guesses)."
  [tgt-word guessed-chars]
  (vec (for [ letter tgt-word ] (guessed-chars letter)) ))

(defn complement-char-class
  "Generate the regex for the complement of a set of characters."
  [not-chars]
  (if (< 0 (count not-chars))
    (str "[^" (str/join not-chars) "]" )  ;normal case
    "."    ; degenerate case of no chars guessed yet - anything matches
  ))

(defn clue-to-regex
  "Convert a clue string and set of guessed chars into a regex for filtering possible
  matching words."
  [clue-str guessed-chars]
  (let [not-guessed-class   (complement-char-class guessed-chars)
        patt-str   (str/join
                     (for [clue-ch clue-str]
                       (if (= clue-ch \-) not-guessed-class clue-ch) ))
    ] patt-str ))

(defn format-clue
  "Format the clue vector into a nice-looking string."
  [clue]
  (str/join 
    (map  #(if (nil? %) "-" % )  clue) ))

(defn do-tests []

  ; Manipulation of strings/vectors/character seq's
  (assert (= (vec "abcd")                [\a \b \c \d] ))
  (assert (= (str/join  (vec "abcd"))    "abcd" ))
  (assert (= (apply str (vec "abcd"))    "abcd" ))

  ; Test regex stuff
  (assert (= (complement-char-class #{})         "."      ))
  (assert (= (complement-char-class #{\a \b})    "[^ab]"  ))
  (let [words           ["abcd" "xbcd" "xxcd" "xxxd"]
        letters         [\a \b \c \d] 
        guessed-chars   #{\b \s}
        clue-str        "-b--"

        not-char-class  (complement-char-class guessed-chars)
        patt-str        (clue-to-regex clue-str guessed-chars)
    ]
    (assert (= not-char-class  "[^bs]"                 ))
    (assert (= patt-str        "[^bs]b[^bs][^bs]"      ))
    (assert      (re-find (re-pattern patt-str) "abcd" ))
    (assert      (re-find (re-pattern patt-str) "xbcd" ))
    (assert (not (re-find (re-pattern patt-str) "xxcd" )))
    (assert (not (re-find (re-pattern patt-str) "xxxd" )))
    (println)
  )

  ; Match guesses
  (assert (guess-matches? "abcd" "abcd") )
  (assert (guess-matches? "abcd" [\a \b \c \d]) )
  (assert (guess-matches? "abcd" [nil nil nil nil]) )
  (assert (guess-matches? "abcd" [\a nil nil nil]) )

  (let [
    tst-words [ "abcd" "xbcd" "xxcd" "xxxd" ] 
    words-map           (group-by count tst-words)
      _ (assert (= words-map   {4 ["abcd" "xbcd" "xxcd" "xxxd"]} ))
    word-list          (words-map 4)
      _ (assert (= word-list     ["abcd" "xbcd" "xxcd" "xxxd"]  ))

    tgt-word       [ \x  \b  \c  \d  ]
    clue           [ nil \b  nil nil ]
    keep-words     (filter #(guess-matches? % clue ) word-list)
      _ (assert (= keep-words ["abcd" "xbcd"] ))
    guessed-chars       #{ \b }
      _ (assert (= guessed-chars #{\b} ))
  ] )
)

(defn main 
  ( [] 
    (main all-words) )
  ( [game-words]
    (do-tests)
    (let [tgt-word      "uniformed"
          words-map     (group-by count game-words)  ; map keyed by word length
          word-list     (words-map (count tgt-word)) ; words of correct length
      ]
      (println)
      (println "************************************************************")
      (println "word-list" (take 20 word-list) )
      (loop [ guessed-chars   #{}
              clue            (make-clue tgt-word guessed-chars) 
            ]
        (println )
        (let [
          keep-words      (filter #(guess-matches? % clue) word-list) ]
            (println "clue: " (format-clue clue) 
                      "  keep-words(" (count keep-words) "):" 
                      (take 10 (map str/join keep-words)) "..." )
            (if (= 1 (count keep-words))
              (let [final-guess (str/join (first keep-words)) ]
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

  ))
)
(defn -main [& args] (apply main args) )

; (defonce sanity-check (do-tests) )
