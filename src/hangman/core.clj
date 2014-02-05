(ns hangman.core
  (:require
    [clojure.string  :as str]
    [clojure.set     :as set]
  ) )

(def show-info-size 10)

(defn show-info
  "Print synopsis info about a sequence of strings"
  [ seqName seqVals ]
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

(defn make-clue
  "Generate a clue given the target word and a vec of guessed letters. The clue is a
  vector the length of the target word consisting of either letters (for correctly guessed
  letters) or nil (for incorrect guesses)."
  [tgt-word guessed-chars]
  (str/join (for [ letter tgt-word ] (get guessed-chars letter \- ))) )

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

(defn filter-words 
  "Filter words retaining only those that are possible matches given the current clue and
  already guessed chars."
  [clue guessed-chars words]
  (let [patt-str      (clue-to-regex clue guessed-chars) 
        re-patt       (re-pattern patt-str)
        keep-words    (filter #(re-find re-patt %) words ) ]
    keep-words ))

(defn make-guess-1
  "Choose the most numerous letter"
  [words used-chars]
  (let [avail-chars   (set/difference all-letters used-chars)
        char-freqs    (frequencies (str/join words))
        best-char     (apply max-key #(get char-freqs % -1) avail-chars) ]
    best-char ))

(defn make-guess
  [words used-chars]
  (make-guess-1 words used-chars) )

(defn do-tests []

  ; Test regex stuff
  (assert (= (complement-char-class #{})         "."      ))
  (assert (= (complement-char-class #{\a \b})    "[^ab]"  ))
  (let [words           ["abcd" "xbcd" "xxcd" "xxxd"]
        letters         [\a \b \c \d] 
        guessed-chars   #{\b \s}
        clue-str        "-b--"
        not-char-class  (complement-char-class            guessed-chars)
        patt-str        (clue-to-regex          clue-str  guessed-chars)
  ]
    (assert (= not-char-class  "[^bs]"                 ))
    (assert (= patt-str        "[^bs]b[^bs][^bs]"      ))
    (assert      (re-find (re-pattern patt-str) "abcd" ))
    (assert      (re-find (re-pattern patt-str) "xbcd" ))
    (assert (not (re-find (re-pattern patt-str) "xxcd" )))
    (assert (not (re-find (re-pattern patt-str) "xxxd" )))

    ; Test if keep word
    (assert (= (filter-words "----"  #{}      ["abcd"] ) ["abcd"] ))
    (assert (= (filter-words "abcd"  #{}      ["abcd"] ) ["abcd"] ))
    (assert (= (filter-words "a---"  #{}      ["abcd"] ) ["abcd"] ))
    (assert (= (filter-words "ab--"  #{}      ["abcd"] ) ["abcd"] ))
    (assert (= (filter-words "-b--"  #{\b \s} ["abcd"] ) ["abcd"] ))
    (assert (= (filter-words "a---"  #{\b \s} ["abcd"] ) []       ))
  )
  (let [tst-words       [ "abcd" "xbcd" "xxcd" "xxxd" ] 
        words-map       (group-by count tst-words)
        word-list       (words-map 4)
  ]
    (assert (= words-map   {4 ["abcd" "xbcd" "xxcd" "xxxd"]} ))
    (assert (= word-list      ["abcd" "xbcd" "xxcd" "xxxd"]  ))
  )
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
      (show-info "word-list" word-list )
      (loop [ guessed-chars   #{}
              clue            (make-clue tgt-word guessed-chars) 
            ]
        (println )
        (let [ keep-words (filter-words clue guessed-chars word-list) ]
          (print "clue: " clue "  ")
          (show-info "keep-words" keep-words)
          (if (= 1 (count keep-words))
            (let [final-guess (str/join (first keep-words)) ]
              (println (str "***** found word:  '" final-guess "'  *****") )
              (println "matches:" (= final-guess tgt-word)) )
          ;else
            (let [new-guess       (make-guess keep-words guessed-chars)
                  guessed-chars   (conj guessed-chars new-guess)
                  new-clue        (make-clue tgt-word guessed-chars)
            ]
              (println "  new-guess" new-guess 
                "  guessed-chars (" (count guessed-chars) ")" guessed-chars)
              (when (some #(= \- %) new-clue)
                (recur  (conj guessed-chars new-guess)  new-clue ) )
            )
          )
        ))

  ))
)
(defn -main [& args] (apply main args) )

; (defonce sanity-check (do-tests) )
