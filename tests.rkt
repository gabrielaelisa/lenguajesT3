#lang play
(require "main.rkt")
(print-only-errors)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                 TESTS BASE                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (run-val '(+ 1 2)) 3)
(test (run-val '(< 1 2)) #t)
(test (run-val '(- 2 1)) 1)
(test (run-val '(* 2 3)) 6)
(test (run-val '(= (+ 2 1) (- 4 1))) #t)
(test (run-val '(and #t #f)) #f)
(test (run-val '(or #t #f)) #t)
(test (run-val '(not (not #t))) #t)
(test (run-val '(if (not #f) (+ 2 1) 4)) 3)
(test (run-val '(local ([define x 5])
              (seqn {+ x 1}
                    x))) 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                  SUS TESTS                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; tests P1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; tests for get

(test (run-val '(local
             [(define Point (class
                          (field x 1)
                          (field y 1)))
              (define p (new Point))] (get p x))) 1)


;;; tests for set

(test (run-val '(local
             [(define Point (class
                          (field x 1)
                          (field y 1)))
              (define p (new Point))] (seqn (set p x 2) (get p x)))) 2)

(test (run-val '(local
             [(define Point (class
                          (field x 1)
                          (field y 2)
                          (method swap () (set this x (get this y)))))
              (define p (new Point))] (seqn (send p swap) (get p x)))) 2)


;; tests for this
; este test no es necesario
#;(test (run-val '(local
             [(define Y (class
                          (field x 1)
                          (field y (get this x))
                          (method  get-y () (get this y))))
              (define c (new Y))] (send c get-y))) 1)

(test (run-val '(local
             [(define O (class
                          (field x 1)
                          (method identidad () (get this x))))
              (define o1 (new O))] (send o1 identidad))) 1)


;; test class with 2 methods
(test (run-val '(local
             [(define Add-x (class
                          (field x 1)
                          (method add-x (y) (+  (get this x) y))
                          (method identidad (x) x)))
              (define a (new Add-x))] (send a add-x (send a identidad 2)))) 3)


(test (run-val '(local
             [(define Swap (class
                          (field x 4)
                          (field y 2)
                          (method swap (x) (set this y x))
                          (method swap-aux (x) (seqn (set this x y) x))))
              (define s (new Swap))] (seqn (send s swap (send s swap-aux (get s x))) (get s y)))) 4)


(test (run-val '(local
             [(define O (class
                          (field x 1)
                          (field y 2)
                          (method swap () (set this x (get this y)))
                          (method identidad () (get this x))))
              (define o1 (new O))] (send o1 identidad))) 1)

(test (run-val '(local
             [(define O (class
                          (field x 1)
                          (field y 2)
                          (method swap () (set this x (get this y)))
                          (method identidad () (get this x))))
              (define o1 (new O))]  (seqn (send o1 swap) (get o1 x)))) 2)


;;; tests for send


(test (run-val '(local
               [(define printer (class
                          (method print () 1)))
              (define pr (new printer))] (send pr print))) 1)



(test (run-val '(local
             [(define O (class
                          (field x 1)
                          (method identidad (x) x)))
              (define ob (new O))] (send ob identidad 1))) 1)

(test (run-val '(local
             [(define O (class
                          (field x 1)
                          (method identidad (x y) x)))
              (define ob (new O))] (send ob identidad 1 2))) 1)

;test por componentes
(test (run-val '(local
             [(define c (class
                            (field x 1)
                          (field y 2)
                          (method sum (z) (+ (get this x) (+ (get this y) z)))
                          (method set-x (val) (set this x val))))
              (define o (new c))]
             (seqn
              (send o set-x (+ 1 3))(get o x)))) 4)

(test (run-val '(local
             [(define c (class
                            (field x 1)
                          (field y 2)
                          (method sum (z) (+ (get this x) (+ (get this y) z)))
                          (method set-x (val) (set this x val))))
              (define o (new c))]
             (send o sum 3))) 6)


;;; test enunciado 1
(test (run-val '(local
             [(define c (class
                            (field x 1)
                          (field y 2)
                          (method sum (z) (+ (get this x) (+ (get this y) z)))
                          (method set-x (val) (set this x val))))
              (define o (new c))]
             (seqn
              (send o set-x (+ 1 3))
              (+ (send o sum 3) (get o y))))) 11)


;;; test las clases son valores

(test (run-val '(local
              [(define A
                 (class
                     (field x 5)
                     (method sub-y (y) (- (get this x) y))))
               (define a (new A))
               (define B
                 (class
                     (field y 5)))
               (define b (new B))
                     ]
              (send a sub-y (get b y)))) 0)

;;; test por componentes

(test (run-val '(local
              [(define A
                 (class
                     (method apply (c)
                             (send (new c) m))))
               (define ins (new A))]
              (send ins apply (class 
                                (method m () 1))))) 1)

;;; test enunciado 2
(test (run-val '(local
              [(define A
                 (class
                     (method apply (c)
                             (send (new c) m))))
               (define ins (new A))]
              (send ins apply (class
                                (field x 2) 
                                (method m () (get this x))))))
2)

;;; testeo de errores

(test/exn (run-val '(local
               [(define printer (class
                          (method print () 1)))
              (define pr (new printer))] (send pr pritt))) "method not found")

(test/exn (run-val '(local
               [(define printer (class
                          (method print () 1)))
              (define pr (new printer))] (get pr x))) "field not found")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;P2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; testings parte 1

; enunciado
(test (run-val '(local
              [(define c1 (class
                              (method f (z) (< z 7))))
               (define c (class <: c1))
               (define o (new c))]
              (send o f 20)))
#f)