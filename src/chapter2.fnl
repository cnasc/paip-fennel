;; Chapter 2 -- A Simple Lisp Program

;; I will just make a translation of the program here and then
;; annotate it in a post highlighting some of the differences.

;; Sentence => Noun-Phrase + Verb-Phrase
;; Noun-Phrase => Article + Noun
;; Verb-Phrase => Verb + Noun-Phrase
;; Article => the, a ...
;; Noun => man, ball, woman, table ...
;; Verb => hit, took, saw, liked ...

(set inspect (require :inspect))

;; Just going to paste this until I know how to handle local imports
(set append
     (fn [...] ; [1]
       "Return a new table containing the concatenation of all tables in `...`"
       (let [result []
             tbls [ ... ]] ; [2]
         (each [i tbl (ipairs tbls)]
           (each [i val (ipairs tbl)]
             (table.insert result val)))
         result)))

(set sentence (fn [] (append (noun-phrase) (verb-phrase))))
(set noun-phrase (fn [] (append (article) (noun))))
(set verb-phrase (fn [] (append (verb) (noun-phrase))))
(set article (fn [] (one-of ["the" "a"])))
(set noun (fn [] (one-of ["man" "ball" "woman" "table"])))
(set verb (fn [] (one-of ["hit" "took" "saw" "liked"])))

(set one-of (fn [tbl]
              "Assuming that tbl is array-like (only integer keys)"
              [ (. tbl (math.random (# tbl))) ]))

(inspect (sentence)) ;; => { "the", "man", "hit", "a", "man" } (will vary)
