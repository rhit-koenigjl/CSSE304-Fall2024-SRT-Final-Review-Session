#lang racket

(require "../chez-init-but-again.rkt")

(provide eval-one-exp)

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

(define eval-one-exp
  (lambda (exp)
    (nyi)))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
