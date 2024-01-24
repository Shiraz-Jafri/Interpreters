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

; Call by need Interpreter
; Handling feature such as lambda functions, conditionals, boolean operators, arithmetic, and mutable variables.

(define val-of-cbneed
  (lambda (exp env) 
    (match exp
      [`,y #:when (symbol? y)
           (let* ([b (apply-env env y)]
                  [v ((unbox b))] 
                  [bv (set-box! b
                                (λ ()
                                  v))])v)]    
      [`,n #:when (number? n)
           n]
      [`,b #:when (boolean? b) 
           b]
      [`(zero? ,n) (zero? (val-of-cbneed n env))] 
      [`(sub1 ,f) (sub1 (val-of-cbneed f env))] 
      [`(* ,n ,e)
       (* (val-of-cbneed n env)  
          (val-of-cbneed e env))] 
      [`(if ,ques ,ans ,else)
       (if (val-of-cbneed ques env)
           (val-of-cbneed ans env)
           (val-of-cbneed else env))]  
      [`(random ,n) 
       (random
        (val-of-cbneed n env))]  
      [`(let ([,x ,e] ,body)) 
       (val-of-cbneed body (extend-env x (box
                                          (λ ()
                                            (val-of-cbneed e env)))
                                       env))]                 
      [`(lambda (,x) ,body) 
       (lambda (arg) (val-of-cbneed body (extend-env x arg env)))] 
      [`(,rator ,var)
       #:when (symbol? var)
       (let* ([b (apply-env env var)]
              [r (val-of-cbneed rator env)])  
         (r b))]     
      [`(,rator ,rand)
       ((val-of-cbneed rator env)
        (box (λ ()
               (val-of-cbneed rand env))))]))) 