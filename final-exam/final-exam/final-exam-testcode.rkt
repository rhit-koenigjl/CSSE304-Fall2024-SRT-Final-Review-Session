#lang racket

; To use these tests:
; Click "Run" in the upper right
; (r)

; If you find errors in your code, fix them, save your file, click the "Run" button again, and type (r)
; You can run a specific group of tests using (run-tests group-name)

(require "../testcode-base.rkt")
(require "final-exam.rkt")
(require "final-exam-interpreter.rkt")
(provide get-weights get-names individual-test test)

; we'll use this when we want to run a typecheck but we expect it to fail
; do to a typing error.  The error symbol is returned.
(define expect-error
  (lambda (code)
    (with-handlers ([symbol? (lambda (x) x)])
      (typecheck code)
      'unexpected-success)))

(define basic-handler identity)

(define ignores 0)
(define aborts 0)
(define retries 0)

(define tracking-handler
   (lambda (s)
      (case s
         ['ignore (begin (set! ignores (add1 ignores)) 'ignore)]
         ['abort (begin (set! aborts (add1 aborts)) 'abort)]
         ['retry (begin (set! retries (add1 retries)) 'retry)])))

(define reset-tracks
   (lambda ()
     (set! ignores 0)
     (set! aborts 0)
     (set! retries 0)))

(define get-tracks
   (lambda ()
      (list ignores aborts retries)))

(define abort-handler
   (lambda x
      'abort))

(define instant-abort
   (lambda () (interrupt 'abort)))

(define ignore-foo
   (lambda () (interrupt 'ignore) 'foo))

(define my-divisor 0)

(define my-exception-handler
  (lambda info (let ([problem (car info)])
     (case problem
       ['div-by-0 (begin (set! my-divisor 4) 'retry)] ; fix the problem then retry
       ['big-number 'ignore] ; just ignore the problem
       ['negative-number 'abort])))) ; kill the program entirely

(define comp-count 0)

(define count-eq?
   (lambda (x y)
      (set! comp-count (add1 comp-count))
      (eq? x y)))

(define same-parity?
   (lambda (x y)
      (eq? (modulo x 2) (modulo y 2))))

(define test (make-test ; (r)
   (typecheck equal?
    [(typecheck 1) 'num 1] ; (run-test typecheck 1)
    [(typecheck #f) 'bool 1] ; (run-test typecheck 2)
    [(typecheck 'zero?) '((num) -> bool) 1] ; (run-test typecheck 3)
    [(typecheck '-) '((num) -> ((num) -> num)) 1] ; (run-test typecheck 4)
    [(typecheck '((- 10) 3)) 'num 1] ; (run-test typecheck 5)
    [(expect-error '(1 1)) 'bad-procedure 1] ; (run-test typecheck 6)
    [(expect-error '(lambda (bool) (f) (f 1))) 'bad-procedure 1] ; (run-test typecheck 7)
    [(typecheck '(lambda (num num) (x y) ((- x) y))) '((num num) -> num) 1] ; (run-test typecheck 8)
    [(typecheck '((lambda (num num) (x y) ((- x) y)) 3 2)) 'num 1] ; (run-test typecheck 9)
    [(typecheck '(lambda (num num) (x y) (zero? ((- x) y)))) '((num num) -> bool) 1] ; (run-test typecheck 10)
    [(typecheck '(letrec num f (lambda (num) (x) (if [zero? x] ((- (f ((- x) 1))) 2) x)) (f 3))) 'num 1] ; (run-test typecheck 11)
    [(typecheck '(lambda (num num) (x y) ((- x) ((- 0) y)))) '((num num) -> num) 1] ; (run-test typecheck 12)
    [(typecheck '(lambda (num num) (x y) (letrec num mult (lambda (num num num) (x y remaining) (if [zero? remaining] 0 ((lambda (num num) (x y) ((- x) ((- 0) y))) (mult x y ((- remaining) 1)) y))) (mult x y x)))) '((num num) -> num) 1] ; (run-test typecheck 13)
    [(typecheck '(lambda (bool num) (my-bool my-num) (if my-bool my-bool (zero? my-num)))) '((bool num) -> bool) 1] ; (run-test typecheck 14)
    [(typecheck '(lambda () () 2)) '(() -> num) 1] ; (run-test typecheck 15)
   )

   (abort-ignore-basic equal? ; (run-test abort-ignore-basic)
      [(abort-retry-ignore basic-handler (lambda () 'foo)) 'foo 1] ; (run-test abort-ignore-basic 1)
      [(abort-retry-ignore basic-handler (lambda () (interrupt 'ignore) 'foo)) 'foo 1] ; (run-test abort-ignore-basic 2)
      [(let ([x 0]) (abort-retry-ignore basic-handler (lambda () (interrupt 'abort) (set! x 1))) x) 0 1] ; (run-test abort-ignore-basic 3)
      [(let ([x 0]) (abort-retry-ignore basic-handler (lambda () (interrupt 'ignore) (set! x 1))) x) 1 1] ; (run-test abort-ignore-basic 4)
      [(let ([x 0]) (abort-retry-ignore basic-handler (lambda () (interrupt 'ignore) (set! x 1) (interrupt 'abort) (set! x 2))) x) 1 1] ; (run-test abort-ignore-basic 5)
      [(begin (reset-tracks) (abort-retry-ignore tracking-handler (lambda () 'foo)) (get-tracks)) '(0 0 0) 1] ; (run-test abort-ignore-basic 6)
      [(begin (reset-tracks) (abort-retry-ignore tracking-handler (lambda () (interrupt 'ignore) 'foo)) (get-tracks)) '(1 0 0) 1] ; (run-test abort-ignore-basic 7)
      [(begin (reset-tracks) (abort-retry-ignore tracking-handler (lambda () (interrupt 'abort) 'foo)) (get-tracks)) '(0 1 0) 1] ; (run-test abort-ignore-basic 8)
      [(begin (reset-tracks) (abort-retry-ignore tracking-handler (lambda () (interrupt 'ignore) (interrupt 'abort) 'foo)) (get-tracks)) '(1 1 0) 1] ; (run-test abort-ignore-basic 9)
      [(begin (reset-tracks) (abort-retry-ignore tracking-handler (lambda () (interrupt 'ignore) (interrupt 'ignore) 'foo)) (get-tracks)) '(2 0 0) 1] ; (run-test abort-ignore-basic 10)
   )

   (abort-ignore-nested equal? ; (run-test abort-ignore-nested)
      [(abort-retry-ignore basic-handler (lambda () (abort-retry-ignore basic-handler ignore-foo) (interrupt 'ignore) 'foo)) 'foo 1] ; (run-test abort-ignore-nested 1)
      [(begin (reset-tracks) (abort-retry-ignore tracking-handler (lambda () (abort-retry-ignore tracking-handler ignore-foo) (interrupt 'abort) 'foo)) (get-tracks)) '(1 1 0) 1] ; (run-test abort-ignore-nested 2)
      [(abort-retry-ignore basic-handler (lambda () (abort-retry-ignore abort-handler instant-abort) (interrupt 'ignore) 'foo)) 'foo 1] ; (run-test abort-ignore-nested 3)
   )

   (abort-retry-ignore-basic equal? ; (run-test abort-retry-ignore-basic)
      [(let ([mynum 200]) (set! my-divisor 0) (abort-retry-ignore my-exception-handler
         (lambda ()
            (if [eq? my-divisor 0]
                (interrupt 'div-by-0)
                (set! mynum (/ mynum my-divisor)))
            (when [> mynum 30]
                (interrupt 'big-number))
            (set! mynum (- mynum 119))
            (when [negative? mynum]
                (interrupt 'negative-number))
            (set! mynum 'BAD)
            )) mynum) -69 1] ; (run-test abort-retry-ignore-basic 1)
      [(begin (reset-tracks) (set! my-divisor 0) (abort-retry-ignore tracking-handler
         (lambda ()
            (let ([mynum 200])
            (if [eq? my-divisor 0]
                (begin (set! my-divisor 4) (interrupt 'retry))
                (set! mynum (/ mynum my-divisor)))
            (when [> mynum 30]
                (interrupt 'ignore))
            (set! mynum (- mynum 119))
            (when [negative? mynum]
                (interrupt 'abort))
            (set! mynum 'BAD))
            )) (get-tracks)) '(1 1 1) 1] ; (run-test abort-retry-ignore-basic 2)
   )

   (abort-retry-ignore-nested equal? ; (run-test abort-retry-ignore-nested)
      [(let ([x 0]) (set! my-divisor 0) (abort-retry-ignore basic-handler (lambda ()
         (abort-retry-ignore basic-handler (lambda () (set! x (add1 x)) (interrupt 'abort)))
         (if [zero? my-divisor]
            (begin (set! my-divisor 1) (interrupt 'retry))
            x)))) 2 1] ; (run-test abort-retry-ignore-nested 1)
      [(begin (reset-tracks) (set! my-divisor 0) (abort-retry-ignore tracking-handler (lambda ()
         (abort-retry-ignore tracking-handler instant-abort)
         (if [zero? my-divisor]
            (begin (set! my-divisor 1) (interrupt 'retry))
            'foo))) (get-tracks)) '(0 2 1) 1] ; (run-test abort-retry-ignore-nested 2)
   )

   (grab-all equal?
      [(eval-one-exp '(grab-all x)) '() 1] ; (run-test grab-all 1)
      [(length (eval-one-exp '(grab-all +))) 1 1] ; (run-test grab-all 2)
      [(eval-one-exp '(let ([x 2]) (grab-all x))) '(2) 1] ; (run-test grab-all 3)
      [(eval-one-exp '(let ([x 2]) (let ([x 3]) (grab-all x)))) '(3 2) 1] ; (run-test grab-all 4)
      [(eval-one-exp '(let ([x 2]) (let ([y 3]) (grab-all x)))) '(2) 1] ; (run-test grab-all 5)
      [(eval-one-exp '(let ([y 2]) (let ([x 3]) (grab-all x)))) '(3) 1] ; (run-test grab-all 6)
      [(eval-one-exp '(let ([x 2]) (let ([x 3]) (set! (grab-all x) 4)) x)) 4 1] ; (run-test grab-all 7)
      [(eval-one-exp '(let ([x 2]) (let ([x 3]) (set! (grab-all x) 4) x))) 4 1] ; (run-test grab-all 8)
      [(eval-one-exp '(let ([x 2]) (let ([x 3]) (set! (grab-all x) 4) (grab-all x)))) '(4 4) 1] ; (run-test grab-all 9)
      [(eval-one-exp '(let ([x 2]) (let ([y 3]) (set! (grab-all x) 4) y))) 3 1] ; (run-test grab-all 10)
      [(eval-one-exp '(let ([x 2]) (let ([x 3]) (set! (grab-all x) (add1 x)) (grab-all x)))) '(4 4) 1] ; (run-test grab-all 11)
      [(eval-one-exp '(let ([x 2]) (let ([x 3]) (set! (grab-all x) (grab-all x)) (grab-all x)))) '((3 2) (3 2)) 1] ; (run-test grab-all 10)
      [(eval-one-exp '(let ([x 2]) (let ([x 3]) (set! x 4) x))) 4 1] ; (run-test grab-all 11)
      [(eval-one-exp '(let ([x 2]) (let ([x 3]) (set! x 4)) x)) 2 1] ; (run-test grab-all 12)
      [(length (eval-one-exp '(let ([+ 2]) (set! + 3) (grab-all +)))) 2 1] ; (run-test grab-all 13)
   )

   (slist-member equal? ; (run-test slist-member)
      [(slist-member eq? 'x ()) #f 1] ; (run-test slist-member 1)
      [(slist-member eq? 'x (x)) #t 1] ; (run-test slist-member 2)
      [(slist-member eq? 'x (y x)) #t 1] ; (run-test slist-member 3)
      [(slist-member eq? 'x (((((x)))))) #t 1] ; (run-test slist-member 4)
      [(slist-member eq? 'x (y y y y y x)) #t 1] ; (run-test slist-member 5)
      [(slist-member eq? 'x ((((()))))) #f 1] ; (run-test slist-member 6)
      [(slist-member eq? 'x (y y y y y y)) #f 1] ; (run-test slist-member 7)
      [(slist-member eq? 'x ((y z a) (b c) (((r f a) ((y t) (u x b) (a x))) (y u t)) (u a s) y)) #t 1] ; (run-test slist-member 8)
   )

   (slist-member-short equal? ; (run-test slist-member-short)
      [(begin (set! comp-count 0) (slist-member count-eq? 'x ()) comp-count) 0 1] ; (run-test slist-member-short 1)
      [(begin (set! comp-count 0) (slist-member count-eq? 'x (x)) comp-count) 1 1] ; (run-test slist-member-short 2)
      [(begin (set! comp-count 0) (slist-member count-eq? 'x (y x y)) comp-count) 2 1] ; (run-test slist-member-short 3)
      [(begin (set! comp-count 0) (slist-member count-eq? 'x ((y x) (a a a))) comp-count) 2 1] ; (run-test slist-member-short 4)
      [(begin (set! comp-count 0) (slist-member count-eq? 'x ((y z a) (b c) (((r f a) ((y t) (u x b) (a x))) (y u t)) (u a s) y)) comp-count) 12 1] ; (run-test slist-member-short 5)
   )

   (grade-tests equal? ; (run-test grade-tests)
      [(grade-tests ('foo 'foo 1)) 1 1] ; (run-test grade-tests 1)
      [(grade-tests ('foo 'bar 1)) 0 1] ; (run-test grade-tests 2)
      [(grade-tests ('foo 'foo 1) ('foo 'bar 1)) 1/2 1] ; (run-test grade-tests 3)
      [(grade-tests ('foo 'foo 1) ('foo 'bar 2)) 1/3 1] ; (run-test grade-tests 4)
      [(grade-tests ('foo 'foo 2) ('foo 'bar 1)) 2/3 1] ; (run-test grade-tests 5)
      [(let ([x 0]) (grade-tests ((set! x (add1 x)) (set! x (add1 x)) 1)) x) 2 1] ; (run-test grade-tests 6)
      [(grade-tests (1 3 same-parity? 1)) 1 1] ; (run-test grade-tests 7)
      [(grade-tests (1 1 same-parity? 1)) 1 1] ; (run-test grade-tests 8)
      [(grade-tests (1 4 same-parity? 1)) 0 1] ; (run-test grade-tests 9)
      [(grade-tests (1 1 1) (1 0 1) (1 1 1) (1 0 1) (1 1 1)) 3/5 1] ; (run-test grade-tests 10)
      [(grade-tests ('foo 'foo 3) ('bar 'baz 2) (4 6 same-parity? 4)) 7/9 1] ; (run-test grade-tests 11)
   )
))

(implicit-run test) ; run tests as soon as this file is loaded
