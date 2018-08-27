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

(test (run-val '(local
             [(define O (class
                          (field x 1)
                          (method identidad () (get this x))))
              (define o1 (new O))] (send o1 identidad))) 1)


;; test class with 2 methods

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
              (define o (new O))] (send o identidad 1))) 1)

(test (run-val '(local
             [(define O (class
                          (field x 1)
                          (method identidad (x y) x)))
              (define o (new O))] (send o identidad 1 2))) 1)

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


;;; test enunciado
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