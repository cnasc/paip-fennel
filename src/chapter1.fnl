;;; PAIP in Fennel -- Chapter 1

;; inspect is a third-party library that prints out Lua tables nicely.
;; Without it, printing a table would result in something like "table:
;; 0x7fa5df500fc0"
(global inspect (require :inspect))

;; Typically it would be considered bad form to use a lot of globals,
;; but due to the way Lua works, it seems impossible to have a nice
;; repl-driven workflow without them. So we'll endeavor to keep them
;; to a minimum and nicely namespace things.

;; These 2 lines will allow us to require Fennel files.
;; package.searchers in Lua 5.2+, package.loaders in Lua 5.1
(global fennel (require :fennel))
(table.insert package.searchers fennel.searcher)

;; Lume is a neat Lua library that is focused on game development, but
;; which has a number of functions we'll find handy.
(global lume (require :lume))

;; 1.1 Symbolic Computation
;; Lists like (list 1 2 3) or '(1 2 3) are fundamental in Common Lisp.
;; Fennel uses tables.
(inspect [1 2 3]) ;; => { 1, 2, 3 }

;; (append '(Pat Kim) '(Robin Sandy)) => (PAT KIM ROBIN SANDY)
;; There is no equivalent to `append` in Lua/Fennel.
;; So let's define one:
(global append
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

(global p [:John :Q :Public]) ;; defining a variable doesn't return anything
(inspect p) ;; => { "John", "Q", "Public" }

(global x 10)
(print (+ x x)) ;; we can use print here, which is the native function
(print (+ x (# p))) ;; `#` returns the length of an array-like table.

;; access members of tables with `.`
(print (. p 1)) ;; => "John" (by the way, array-like tables are indexed from 1)

(global copy
     (fn [tbl]
       (let [result []]
         (each [i v (ipairs tbl)]
           (table.insert result v))
         result)))

(global cons
     (fn [val tbl]
       (let [result (copy tbl)]
         (table.insert result 1 val)
         result)))

;; 1.7 Higher Order Functions
;; There is no mapcar (or map) in Lua/Fennel; best practice is
;; probably to use a library, but just for fun we'll implement a basic
;; (only operates on one table) version of it

(global map
     (fn [f tbl]
       "Creates a new table from the results of applying `f` to each element of tbl"
       (let [result []]
         (each [i v (ipairs tbl)]
           (table.insert result (f v)))
         result)))

(global mappend
     (fn [f tbl]
       "Apply f to each element of the list and append the results"
       (append (table.unpack (map f tbl)))))

(global self-and-double
     (fn [x]
       [ x (+ x x) ]))

(inspect (map self-and-double [1 10 300])) ; => { { 1, 2 }, { 10, 20 }, { 300, 600 } }
(inspect (mappend self-and-double [1 10 300])) ; => { 1, 2, 10, 20, 300, 600 }

;; EXERCISE:
;; Given a list of elements, return a list consisting of all the
;; numbers in the original list and the negation of those numbers.

(global number-and-negation
     (fn [x]
       (if (= (type x) :number)
         [ x (- x) ]
         [])))

(inspect (mappend number-and-negation [:testing 1 2 3 :test])) ; => { 1, -1, 2, -2, 3, -3 }

;; 1.11 Exercises

;; 1.2 Write a function to exponentiate a number to an integer power
;; ex. (power 3 2) => 9

(global even?
     (fn [x]
       (= (% x 2) 0)))

(global power-cheat (fn [b e] (^ b e)))

(global power-rec
     (fn [b e]
       (if (= e 0) 1
           (even? e) (let [x (power-rec b (/ e 2))]
                       (* x x))
           :else (* b (power-rec b (- e 1))))))

(global power-iter
     (fn [b e]
       (if (even? e) (let [x (power-iter b (/ e 2))]
                       (* x x))
           (let [result 1]
             (for [i 1 e]
               (set result (* result b)))
             result))))

;; Lua/Fennel has proper tail calls!
(global power-tail
     (fn [b e ?acc]
       (if (<= e 0) (or ?acc 1)
           (even? e) (let [x (power-tail b (/ e 2))]
                       (* x x (or ?acc 1)))
           :else (power-tail b (- e 1) (* (or ?acc 1) b)))))


;; 1.3 Write a function that counts the number of atoms in an
;; expression. In Common Lisp, nil is both an atom and a list. Not so
;; in Fennel.
;; ex. (count-atoms [:a [:b] :c]) => 3
(global table? (fn [x] (= (type x) :table)))

(global count-atoms
     (fn [tbl]
       (let [result 0]
         (each [i v (ipairs tbl)]
           (if (table? v)
             (set result (+ result (count-atoms v)))
             (set result (+ result 1))))
         result)))

;; 1.4 Write a function that counts the number of times an expression
;; occurs anywhere within another expression. NB: I interpret this to
;; mean counting the number of times a given atom appears in an
;; expression.
;; ex. (count-anywhere :a [:a [[:a] :b] :a]) => 3

(global count-anywhere
     (fn [needle haystack]
       (let [result 0]
         (each [i v (ipairs haystack)]
           (if (= v needle) (set result (+ result 1))
               (table? v) (set result (+ result (count-anywhere needle v)))))
         result)))

;; See that each pattern in these last 2? Good spot for a dolist-esque macro. TODO

;; 1.5 Write a function to compute the dot product of two sequences of numbers.
;; (dot-product [10 20] [3 4]) => 110

(global dot-product
     (fn [s1 s2]
       (let [result 0]
         (for [i 1 (# s1)]
           (set result (+ result (* (. s1 i) (. s2 i)))))
         result)))

|# 

Thoughts after one chapter:

Fennel is a cool language. It is also a very different language from
Common Lisp (of course).
Key differences:
- Very small standard library (both a strength and a liability)
- This implies no mapcar, remove-if-not, and friends without
  using a library or rolling your own.
- Iteration over tables is much better supported than recursion
- I used the fn style of function declaration, which pretty much
  translates 1:1 to Lua's function declaration. Apparently the
  lambda style has some runtime overhead. How much? Would be 
  interesting to benchmark them.
  What should the idiom be? Prefer lambda for safety and only
  drop down to fn for frequently called functions after profiling?

Repl workflow:
- Rather than evaluating on a function-by-function basis (ala Slime,
  etc.), I think that the best experience would be to write code as
  namespaced modules and then have a workspace where they are required
  into global tables and then hotswapped after changes are made. It's
  not the prettiest thing, but it works.

#|
