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

; call by value Interpreter
; Handling cases such as arithmetic operations, conditionals, random, let expressions, and modifying mutable variables.

(define val-of-cbv
  (lambda (exp env) 
    (match exp
      [`,y #:when (symbol? y)
           (unbox (apply-env env y))] 
      [`,n #:when (number? n)
           n]
      [`,b #:when (boolean? b) 
           b]
      [`(zero? ,n) (val-of-cbv n env)]
      [`(sub1 ,f) (sub1 (val-of-cbv f env))] 
      [`(* ,n ,e)
       (* (val-of-cbv n env)  
          (val-of-cbv e env))] 
      [`(if ,ques ,ans ,else)
       (if (val-of-cbv ques env)
           (val-of-cbv ans env)
           (val-of-cbv else env))]  
      [`(random ,n) (random (val-of-cbv n))] 
      [`(let ([,x ,e] ,body)) 
       (val-of-cbv body
                   (extend-env x
                               (box (val-of-cbv e env)) env))]     
      [`(set! ,y ,exp)   
       (let* ([val (val-of-cbv exp env)]
              [b (apply-env env y)]
              [_ (set-box! b val)])   
         val)]           
      [`(begin2 ,e1 ,e2)
       (let* ([_ (val-of-cbv e1 env)] 
              [t (val-of-cbv e2 env)] 
              ) t)]         
      [`(lambda (,x) ,body) 
       (lambda (arg) (val-of-cbv body (extend-env x arg env)))]
      [`(,rator ,rand)
       ((val-of-cbv rator env)
        (box (val-of-cbv rand env)))])))

(test (val-of-cbv
       '((lambda (a)
           ((lambda (p)
              (begin2
                (p a)
                a)) (lambda (x) (set! x 4)))) 3)
       (empty-env))
      3)

(test (val-of-cbv
       '((lambda (f)
           ((lambda (g)
              ((lambda (z) (begin2
                             (g z)
                             z))
               55))
            (lambda (y) (f y)))) (lambda (x) (set! x 44)))
       (empty-env))
      55) 