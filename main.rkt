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
  
  (my-class members)
  (my-new o)
  (my-set expr1 id expr2)
  (my-get expr id)
  (my-send obj m expr)
  (my-this)
  
  )

;; values
(deftype Val
  (numV n)
  (boolV b))

(deftype Def
  (my-def id expr))

(deftype Member
  (fld id body)
  (mthd m params body)
  )

;;;auxiliary type
(deftype Group
  (group flds mthds))

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
(define (parse s-expr)
   ;; parse-member:: s-expr-> Member
  (define (parse-member s-expr)
    (match s-expr
      [(list 'field f init) (fld f init )]
      [(list 'method m params body)(mthd m params (parse body))]
      ))
  (match s-expr
    [(? number?) (num s-expr)]
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
    [(list 'class member ...) (my-class (map parse-member member)) ]
    [(list 'this) (my-this)]
    [(list 'new o) (my-new o)]
    [(list 'set expr1 id expr2) (my-set expr1 id expr2)]
    [(list 'get e id) (my-get (parse e) id)]
    [(list 'send expr1 id expr2 ...) (my-send expr1 id (map parse expr2))]
    [(list 'seqn e1 e2) (seqn (parse e1) (parse e2))]    
    [(list 'local (list e ...)  b)
     (lcal (map parse-def e) (parse b))]
    ))


;; parse-def :: s-expr -> Def
(define (parse-def s-expr)
  (match s-expr
    [(list 'define id b) (my-def id (parse b))]))


;; interp :: Expr Env -> Val
(define (interp expr env)
  (match expr
    [(num n) (numV n)]    
    [(bool b) (boolV b)]    
    [(binop f l r) (make-val (f (open-val (interp l env))
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
    
    [(my-send o m expr)((obj-class (env-lookup o env)) 'invoke o 'm
                                                       (map (λ (e) (interp e env)) expr))]
    [(my-new o) ((env-lookup o env) 'create)]
    
    [(my-class members) (def (group fields methods)
                     (separate-members members '() '() ))
                       (letrec
                           ([class
                                (λ (msg . vals)
                                  (case msg
                                    [(create)
                                     (make-obj class
                                               (make-hash fields))]
                                    #;[(read)
                                     (dict-ref (obj-values (first vals)) (second vals))]
                                    #;[(write)
                                     (dict-set! (obj-values (first vals)) (second vals) (third vals))]
                                    [(invoke)
                                     ;(displayln (second vals))
                                     (displayln (cdr (assoc (second vals) methods)))
                                     (if (assoc (second vals) methods)
                                         (apply (cdr (assoc (second vals) methods)) (cddr vals))
                                         (error "message not understood"))]))])
                         class)

                    ]
    [(lcal defs body)
     (let* ([new-env (multi-extend-env '() '() env)])
       (for-each (λ(x)
                   (let ([in-def (interp-def x new-env)])
                     (extend-frame-env! (car in-def) (cdr in-def) new-env)
                     #t)) defs)       
       (interp body new-env))     
     ]
    ))

;;separate-members:: list<Member> '() '() -> (Group list<fld> list<mthd>)
(define (separate-members mem acc_f acc_m)
  (match mem
    ['() (group acc_f acc_m)]
    [(list head tail ...)
     (match head
       [(fld id body) (separate-members tail (cons (cons id body) acc_f) acc_m) ]
       [(mthd m params body) (separate-members tail acc_f (append (list (cons 'm (λ params body))) acc_m))])
     ]))

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
  (define val (interp (parse s-expr) empty-env))
  (match val
    [(numV n) n]
    [(boolV b) b]
    [x x]))

;;;;;;;;;;;;;;;;; TAREA 3;;;;;;;;;;;;

(defmac (-> o m arg ...)
  (let ((obj o))
    ((obj-class obj) 'invoke obj 'm arg ...)))
(defmac (? fd) #:captures self
  ((obj-class self) 'read self 'fd))
 
(defmac (! fd v) #:captures self
  ((obj-class self) 'write self 'fd v))

;(define (new c) (c 'create))
