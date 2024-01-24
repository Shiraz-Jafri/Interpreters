#lang racket

;;tester

(define test
  (λ (a1 a2)
    (when (not (equal? a1 a2)) (raise-user-error 'b "bad"))))

;; Environment Functions


; Simple function used for storing variables in current environment.
(define apply-env
  (λ (env var)
    (env var))) 

; Function used for extending given environment.

(define extend-env
  (λ (id val env)
    (λ (var) 
      (if (eqv? var id) 
          val (env var)))))

; Function that acts as an empty environment used mostly for testing.

(define empty-env
  (λ ()
    (λ (x)
      (error "Unbound identifier" x))))

; Call by Reference Interpreter
; Handling cases such as arithmetic operations, conditionals, random, lambda functions, mofidying mutable variables, function calls.

(define val-of-cbr
  (lambda (exp env) 
    (match exp
      [`,y #:when (symbol? y)
           (unbox (apply-env env y))] 
      [`,n #:when (number? n)
           n]
      [`,b #:when (boolean? b) 
           b]
      [`(zero? ,n) (val-of-cbr n env)]
      [`(sub1 ,f) (sub1 (val-of-cbr f env))] 
      [`(* ,n ,e)
       (* (val-of-cbr n env)  
          (val-of-cbr e env))] 
      [`(if ,ques ,ans ,else)
       (if (val-of-cbr ques env)
           (val-of-cbr ans env)
           (val-of-cbr else env))]  
      [`(random ,n) (random (val-of-cbr n))] 
      [`(let ([,x ,e] ,body)) 
       (val-of-cbr body (extend-env x (box (val-of-cbr e env)) env))]     
      [`(set! ,y ,exp)   
       (let* ([val (val-of-cbr exp env)]
              [b (apply-env env y)]
              [_ (set-box! b val)])   
         val)]           
      [`(begin2 ,e1 ,e2)
       (let* ([_ (val-of-cbr e1 env)] 
              [t (val-of-cbr e2 env)] 
              ) t)]         
      [`(lambda (,x) ,body) 
       (lambda (arg) (val-of-cbr body (extend-env x arg env)))]
      [`(,rator ,var)
       #:when (symbol? var)
       (let* ([b (apply-env env var)]
              [r (val-of-cbr rator env)]) 
         (r b))]     
      [`(,rator ,rand)
       ((val-of-cbr rator env)
        (box (val-of-cbr rand env)))   
       ])))

(println "Call by Reference")

(test (val-of-cbr 
       '((lambda (x) (begin2 (set! x #t)
                             (if x 3 5))) #f)
       (empty-env)) 
      3)

(test (val-of-cbr
       '((lambda (a)
           ((lambda (p)
              (begin2
                (p a)
                a)) (lambda (x) (set! x 4)))) 3)
       (empty-env))
      4)

(test (val-of-cbr
       '((lambda (f)
           ((lambda (g)
              ((lambda (z) (begin2
                             (g z)
                             z))
               55))
            (lambda (y) (f y)))) (lambda (x) (set! x 44)))
       (empty-env))
      44) 