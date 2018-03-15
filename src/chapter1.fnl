;;; PAIP in Fennel -- Chapter 1

;; `set` creates global variables
;; It's probably better to use local variables, but for the sake of
;; readability, I will pollute the global namespace here a bit.
;; Lua/Fennel doesn't print tables readably by default, so I've pulled
;; in a third-party library to allow us to see our operations on tables.
(set inspect (require :inspect))

;; 1.1 Symbolic Computation
;; Lists like (list 1 2 3) or '(1 2 3) are fundamental in Common Lisp.
;; Fennel uses tables.
(inspect [1 2 3]) ;; => { 1, 2, 3 }

;; (append '(Pat Kim) '(Robin Sandy)) => (PAT KIM ROBIN SANDY)
;; There is no equivalent to `append` in Lua/Fennel.
;; So let's define one:
(set append
     (fn [...] ; [1]
         "Return a new table containing the concatenation of all tables in `...`"
         (let [result []
               tbls [ ... ]] ; [2]
           (each [i tbl (ipairs tbls)] 
                 (each [i val (ipairs tbl)] 
                       (table.insert result val)))
           result)))

;; [1] `...` is a special parameter that allows you to pass many
;; arguments

;; [2] [ ... ] packs the arguments (however many there are) into a
;;     table.

;; [3] So we have a table of tables. We insert each child element of
;;     each argument into the result table.

(inspect (append [:Pat :Kim] [:Robin :Sandy])) ; => { "Pat", "Kim", "Robin", "Sandy" }

;; there is no `quote`, use string or :keyword-syntax (which is
;; shorthand for a string with no spaces)
(inspect :hot-dogs) ;; => "hot-dogs"

(set p [:John :Q :Public]) ;; defining a variable doesn't return anything
(inspect p) ;; => { "John", "Q", "Public" }

(set x 10)
(print (+ x x)) ;; we can use print here, which is the native function
(print (+ x (# p))) ;; `#` returns the length of an array-like table.

;; access members of tables with `.`
(print (. p 1)) ;; => "John" (by the way, array-like tables are indexed from 1)

(set copy (fn [tbl]
              (let [result []]
                (each [i v (ipairs tbl)]
                      (table.insert result v))
                result)))

(set cons (fn [val tbl]
              (let [result (copy tbl)]
                (table.insert result 1 val)
                result)))
