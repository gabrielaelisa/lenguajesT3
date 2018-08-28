#lang play

#|
<expr> ::= <num>
         | <id>
         | <bool>
         | (if <expr> <expr> <expr>)
         | (+ <expr> <expr>)
         | '< <s-expr> <s-expr>)
         | (* <s-expr> <s-expr>)
         | (= <s-expr> <s-expr>)
         | (- <s-expr> <s-expr>)
         | (and <s-expr> <s-expr>)
         | (or <s-expr> <s-expr>)
         | (not <s-expr> <s-expr>)         
         | (seqn <expr> <expr>)
         | (local { <def> ...} <expr>)

<def>    ::= (define <id> <expr>)

;EXTENSION PARA CLASE Y OBJETOS
<expr>  ::= ... (todo lo anterior)        
         | (class <member> ...)
         | (class <: <expr> <member> ...)
         | (new <expr>)
         | this
         | (super id <expr> ...)
         | (set <expr> <id> <expr>)
         | (get <expr> <id>)
         | (send <expr> <id> <expr> ...)

<member> ::= 
        | (field <id> <s-expr>)
        | (method <id> (list <id> ...) <s-expr>)

|#

(deftype Expr
  (num n)
  (bool b)
  (id s)   
  (binop f l r)
  (unop f s)
  (my-if c tb fb)  
  (seqn expr1 expr2)  
  (lcal defs body)
  ;;;;;;;;;;;;;;;
  ;; definiciones para  clases
  
  (my-class sc members)
  (my-new o)
  (my-set expr1 id expr2)
  (my-get expr id)
  (my-send obj m expr)
  (my-this)
  (my-super id exprs)
  
  )

;; values
(deftype Val
  (numV n)
  (boolV b)
  (closureV val env))

(deftype Def
  (my-def id expr))

(deftype Member
  (fld id body)
  (mthd m params body)
  )

;;;auxiliary type
(deftype Group
  (group flds mthds))
;struc<obj>: function x(pair)
(define-struct obj (class values))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
Environment abstract data type
 
empty-env        :: Env
env-lookup       :: Sym Env -> Val
multi-extend-env :: List<Sym> List<Val> Env -> Env
extend-frame-env! :: Sym Val Env -> Env 


representation BNF:
<env> ::= (mtEnv)
        | (aEnv <id> <val> <env>)
|#

(deftype Env
  (mtEnv)
  (aEnv hash env)) 

(def empty-env (mtEnv))

#|
env-lookup:: Sym Env -> Val
Busca un símbolo en el ambiente, retornando su valor asociado.
|#
(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(aEnv hash rest)
     (if (hash-has-key? hash x)
         (hash-ref hash x)
         (env-lookup x rest))]))

#|
multi-extend-env:: List(Sym) List(Expr) Env -> Env
Crea un nuevo ambiente asociando los símbolos a sus valores.
|#
(define (multi-extend-env ids exprs env)
  (if (= (length ids) (length exprs))
      (aEnv (make-hash (map cons ids exprs)) env)
      (error "wrong_input, mismatched lengths")))

#|
extend-frame-env!:: Sym Val Env -> Void
Agrega un nuevo par (Sym, Val) al ambiente usando mutación.
Este método no crea un nuevo ambiente.
|#
(define (extend-frame-env! id val env)
  (match env
    [(mtEnv) (aEnv (make-hash (list (cons id val))) env)]
    [(aEnv h rEnv) (let* ([l (hash->list h)]
                          [la (cons (cons id val) l)])
                     (set-aEnv-hash! env (make-hash la)))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; parse :: s-expr -> Expr
;; parsea una s-expr
(define (parse s-expr)
  
   ;; parse-member:: s-expr-> Member
  (define (parse-member s-expr)
    (match s-expr
      [(list 'field f init) (fld f (parse init))]
      [(list 'method m params body)(mthd m params (parse body))]
      ))
  (match s-expr
    [(? number?) (num s-expr)]
    ['this (my-this)]
    [(? symbol?) (id s-expr)]    
    [(? boolean?) (bool s-expr)]
    [(list '* l r) (binop * (parse l) (parse r))]
    [(list '+ l r) (binop + (parse l) (parse r))]
    [(list '- l r) (binop - (parse l) (parse r))]
    [(list '< l r) (binop < (parse l) (parse r))]
    [(list '= l r) (binop = (parse l) (parse r))]    
    [(list 'or l r) (binop (λ (i d) (or i d)) (parse l) (parse r))]
    [(list 'and l r) (binop (λ (i d) (and i d)) (parse l) (parse r))]
    [(list 'not b) (unop not (parse b))]
    [(list 'if c t f) (my-if (parse c)
                             (parse t)
                             (parse f))]
    [(list 'super id exprs ...) (my-super id (map parse exprs))]
    [(list 'class '<: sc-name member ...) (my-class (parse sc-name) (map parse-member member))]
    [(list 'class member ...) (my-class (id 'Object) (map parse-member member)) ]
    [(list 'new o) (my-new (parse o))]
    [(list 'set expr1 id expr2) (my-set (parse expr1) id (parse expr2))]
    [(list 'get e id) (my-get (parse e) id)]
    [(list 'send ob message expr2 ...) (my-send (parse ob) message (map parse expr2))]
    [(list 'seqn e1 e2) (seqn (parse e1) (parse e2))]    
    [(list 'local (list e ...)  b)
     (lcal (map parse-def e) (parse b))]
    ))


;; parse-def :: s-expr -> Def
(define (parse-def s-expr)
  (match s-expr
    [(list 'define id b) (my-def id (parse b))]))


;; interp :: Expr Env -> Val
;;interpreta una expresion dada 
(define (interp expr env)
  
 ;entrega la última aparicion en el ambiente
 (define (get-last x acc env)
   (match env
    [(mtEnv) acc]
    [(aEnv hash rest)
     (if (hash-has-key? hash x)
         (get-last x (hash-ref hash x) rest)
         (get-last x acc rest))]))

  
  ;;separate-members:: list<Member> '() '() -> (Group list<fld> list<mthd>)
  (define (separate-members mem acc_f acc_m)
    (match mem
      ['() (group acc_f acc_m)]
      [(list head tail ...)
       (match head
         [(fld id body) (separate-members tail (cons (cons id body) acc_f) acc_m) ]
         [(mthd m params body) (separate-members tail acc_f (append (list (cons m (list params body))) acc_m))])
       ]))
  
  (match expr
    [(num n) (numV n)]    
    [(bool b) (boolV b)]    
    [(binop f l r)(make-val (f (open-val (interp l env))
                                (open-val (interp r env))))]
    [(unop f s) (make-val (f (open-val (interp s env))))]
    [(my-if c t f)
     (def (boolV cnd) (interp c env))
     (if cnd
         (interp t env)
         (interp f env))]
    [(id x) (env-lookup x env)]        
    [(seqn expr1 expr2) (begin 
                          (interp expr1 env)
                          (interp expr2 env))]
    
    [(my-this) (get-last 'this '() env)]
    
    [(my-set o field val) ((obj-class (interp o env))
                                               'write (interp o env) field (closureV val env))]
    
    [(my-get o field)(match (interp o env)
                          [(obj class values)((obj-class (interp o env)) 'read (interp o env) field)]
                          [_((interp o env) 'read ((interp o env) 'create) field)]
                          )]

    [(my-super m expr) (def superclass (interp (id 'super) env))
                         (superclass  'invoke superclass m
                                                       (map (λ (e) (interp e env)) expr))
                         ]
    
    [(my-send objc m expr)((obj-class (interp objc env)) 'invoke (interp objc env) m
                                                       (map (λ (e) (interp e env)) expr))]
    [(my-new o) ((interp o env) 'create)]
    
    [(my-class sc members) (def (group flds methods)
                     (separate-members members '() '() ))
                      (def my-sc (interp sc env))
                      (def sclass (my-sc 'create))
                      (def fields (cons (cons sc 'all-fields) flds))
                       (letrec
                           ([class
                                (λ (msg . vals)
                                  (case msg
                                    [(create)
                                     (make-obj class
                                               (make-hash fields))]
                                    
                                    [(read)
                                    (let
                                     ([dict (obj-values (first vals))]) 
                                     (if (hash-has-key? dict (second vals))
                                     (match (dict-ref dict (second vals))
                                       [(closureV val my-env) (interp val my-env)]
                                       [ some (interp some env)])
                                     (my-sc 'read sclass (second vals))) )]
                                     
                                    
                                    [(write)
                                     (dict-set! (obj-values (first vals)) (second vals) (third vals))]
                                    
                                    [(invoke)
                                      (let ((method (class 'lookup (second vals))))
                                        (match method
                                         [(list params body)(interp body
                                            (multi-extend-env (append (list 'this 'super) params)
                                             (append(list (first vals) my-sc) (car(cddr vals))) env))]))]
                                      
                                    [(lookup)
                                     (let ([found (assoc (first vals) methods)])
                                       ;(displayln found)
                                       (if found
                                           (cdr found)
                                           (my-sc 'lookup (first vals))))]
                                    ))])
                         class)]
    [(lcal defs body)
     (let* ([new-env (multi-extend-env '() '() env)])
       (for-each (λ(x)
                   (let ([in-def (interp-def x new-env)])
                     (extend-frame-env! (car in-def) (cdr in-def) new-env)
                     #t)) defs)       
       (interp body new-env))     
     ]
    ))



;; open-val :: Val -> Scheme Value
(define (open-val v)
  
  (match v
    [(numV n) n]
    [(boolV b) b]
    ))

;; make-val :: Scheme Value -> Val
(define (make-val v)
  (match v
    [(? number?) (numV v)]
    [(? boolean?) (boolV v)]
    ))

;; interp-def :: Def, Env -> Expr
(define (interp-def a-def env)
  (match a-def
    [(my-def id body) (cons id (interp body env))]))

;; run :: s-expr -> Val
(define (run s-expr)
  (interp (parse s-expr) empty-env))

#|
run-val:: s-expr -> Scheme-Val + Val
Versión alternativa de run, que retorna valores de scheme para primitivas y
valores de MiniScheme para clases y objetos
|#
(define (run-val s-expr)
  (define val (interp (parse s-expr)
                      (extend-frame-env! 'Object
                                         (λ (msg . vals)
                                           (case msg
                                             [(all-fields) '()]
                                             [(lookup-field)(error "field not found")]
                                             [(read) (error "field not found")]
                                             [(lookup) (error "method not found")]
                                             [(create) '()]
                                             [else     (error "root class: should not happen: " msg)]))
                      empty-env)))
  (match val
    [(numV n) n]
    [(boolV b) b]
    [x x]))
