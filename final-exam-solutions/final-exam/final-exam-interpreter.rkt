#lang racket

(require "../chez-init-but-again.rkt")

(provide eval-one-exp)

;; NAT'S SOLUTION
;; I can't give you a working interpreter, but here's what I modified

;; QUESTION 4 Interpreter

;; Make your interpreter support a new expression, grab-all. It's
;; called with a symbol, like (grab-all x). It returns the list of
;; values that x is bound to, starting at the most recent binding.
;; For example:
; (let ([x 2])
;   (let ([x 3])
;     (grab-all x)))
; => '(3 2)

;; Additionally, you should be able to call set! on (grab-all x) to
;; set! all the bindings of x to the same value across all environments.
;; For example:
; (let ([x 2])
;   (let ([x 3])
;     (set! (grab-all x)) 4)
;   x)
; => 4 (the value of x in the top-most environment was also set to 4, but the interesting part is that the other one changed)

;; I've put a stub in so the tests run, but you should paste in your interpreter.

;-------------------+
;                   |
;   sec:DATATYPES   |
;                   |
;-------------------+

(define-datatype expression expression?
  [grab-all-exp
   (id symbol?)]
  [grab-all-set!-exp
   (grab-all-exp expression?)
   (exp expression?)]
  )

(define parse-exp         
  (lambda (datum)
    (cond
      [(list? datum)
       (cond
         [(eqv? (car datum) 'grab-all) (grab-all-exp (second datum))]
         [(eqv? (car datum) 'set!)
          (cond [(not (= (length datum) 3)) (error 'parse-exp "parse error set should have two arguments: ~s" datum)]
                [(not (symbol? (car datum))) (error 'parse-exp "parse error set first argument should be an identifier: ~s" datum)]
                [else (with-handlers ([exn:parse?
                                       (lambda (x) (error 'parse-exp "parse error set! second arg not a valid expression: ~s" datum))])
                        (if [symbol? (2nd datum)]
                            (set!-exp (2nd datum) (parse-exp (3rd datum)))
                            (grab-all-set!-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)))))])]
      [else (error 'parse-exp "bad expression: ~s" datum)])))

(define apply-env-all
  (lambda (env sym)
    (map unbox (apply-env-all-ref env sym))))

(define apply-env-all-ref
  (lambda (env sym)
    (cases environment env
      [empty-env-record ()
                        (apply-global-env-all-ref global-env sym)]
      [extended-env-record (syms vals env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (cons (list-ref vals pos) (apply-env-all-ref env sym))
                                 (apply-env-all-ref env sym)))])))

(define apply-global-env-all-ref
  (lambda (env sym)
    (cases environment env
      [extended-env-record (syms vals env)
                           (let ([pos (list-find-position sym syms)])
                             (if (number? pos)
                                 (list (list-ref vals pos))
                                 (list)))]
      [empty-env-record ()
                        (error 'global-env "This should never happen")])))
						
(define eval-exp
  (let ([identity-proc (lambda (x) x)])
   (lambda (exp env)
    (cases expression exp
      [grab-all-exp (id) (apply-env-all env id)]
      [grab-all-set!-exp (grab-all-e exp)
                         (cases expression grab-all-e
                           [grab-all-exp (id)
                                         (let ([set-value (eval-exp exp env)])
                                           (map (lambda (env-box) (set-box! env-box set-value))
                                                (apply-env-all-ref env id)))]
                           [else (raise 'bad-set!-exp)])]

(define syntax-exp
   (lambda (exp)
    (cases expression exp
      [grab-all-exp (id) (grab-all-exp id)]
      [grab-all-set!-exp (grab-all-exp exp) (grab-all-set!-exp (syntax-exp grab-all-exp) (syntax-exp exp))]
