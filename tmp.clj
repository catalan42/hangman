  ; Match guesses
  (println "match guesses" )
  (assert (match-guess? "abcd" "abcd") )
  (assert (match-guess? "abcd" [\a \b \c \d]) )
  (assert (match-guess? "abcd" [nil nil nil nil]) )
  (assert (match-guess? "abcd" [\a nil nil nil]) )
