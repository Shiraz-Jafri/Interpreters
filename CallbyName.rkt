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

; Call by name Interpreter
; Handling features such as Boolean operators, lambda functions, conditionals, mutable variables.

(define val-of-cbname
  (lambda (exp env) 
    (match exp
      [`,y #:when (symbol? y)
           ((unbox (apply-env env y)))]  
      [`,n #:when (number? n)
           n]
      [`,b #:when (boolean? b) 
           b]
      [`(zero? ,n) (zero? (val-of-cbname n env))] 
      [`(sub1 ,f) (sub1 (val-of-cbname f env))] 
      [`(* ,n ,e)
       (* (val-of-cbname n env)  
          (val-of-cbname e env))] 
      [`(if ,ques ,ans ,else)
       (if (val-of-cbname ques env)
           (val-of-cbname ans env)
           (val-of-cbname else env))]  
      [`(random ,n)
       (random (val-of-cbname n env))]  
      [`(let ([,x ,e] ,body)) 
       (val-of-cbname body (extend-env x (box
                                          (λ ()
                                            (val-of-cbname e env)))
                                       env))]                 
      [`(lambda (,x) ,body) 
       (lambda (arg) (val-of-cbname body (extend-env x arg env)))] 
      [`(,rator ,var)
       #:when (symbol? var)
       (let* ([b (apply-env env var)]
              [r (val-of-cbname rator env)])  
         (r b))]     
      [`(,rator ,rand)
       ((val-of-cbname rator env)
        (box (λ ()
               (val-of-cbname rand env))))      
       ]))) 