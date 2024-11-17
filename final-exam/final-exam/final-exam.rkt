#lang racket

(require "../chez-init.rkt")

(provide typecheck abort-retry-ignore interrupt slist-member grade-tests)

;; QUESTION 1 typing
;;
;; Using your A19 homework as a start, implement multivariable lambdas. If you haven't done A19 yet,
;; use this time to get started on it. Nat doesn't know if A19 is going to be used
;; as starter code on the exam, but he does know that completing A19 will greatly improve
;; your chances of completing Q2 on the final.

;; A lambda used to be implemented like (lambda param-type (param-name) body), and its type
;; would be printed as (param-type -> body-type). Now, all lambdas (so the old syntax is invalid)
;; are implemented like (lambda (param-type*) (param-name*) body), where there are as few as 0 param types
;; and param names, and the count has to be equal. We print out lambda types like ((param-type*) -> body-type).

;; Example: (lambda (bool num) (my-bool my-num) (if my-bool my-bool (zero? my-num))) is printed as
;; ((bool num) -> bool).

;; You'll need to modify lambda-exp, app-exp, the parsing of those, the typechecking of those, and your environment.
;; Don't worry about making up new error codes, but keep the old ones working in the same way.

;; Paste in A19 here
(define typecheck
  (lambda (code)
    (nyi)))

;; QUESTION 2 call/cc
;;
;; Remember CSSE232 exception handling? If you haven't taken it or have pushed it out of your mind,
;; don't worry too much. Basically, here's the main idea:
;;
;; When something BAD happens (e.g., divide by 0, overflow, bad memory access), the architecture
;; gives control to something called the "exception handler". The exception handler does some
;; detective work to figure out what the problem is, and let's say it has 3 options:
;;
;;     1. The entire execution is ABORTED
;;     2. The problematic statement is RETRIED (usually after the exception handler changes something)
;;     3. The problematic statement is IGNORED and skipped
;;
;; For those of you interested in lore, more info about the motivation for this question can be
;; found at https://en.wikipedia.org/wiki/Abort,_Retry,_Fail
;;
;; You will write two procedures:
;; 
;; 1. abort-retry-ignore, which takes two arguments. The first is the exception handler (explained
;; later), and the second is the code to be run in thunk form. The code may call the procedure
;; "interrupt" at some point in its execution. If the code is not aborted, the return value
;; of the thunk should be abort-retry-ignore's return value.
;;
;; 2. interrupt, which takes an any number of arguments. The argument(s) are information that
;; should be passed to the exception handler. The exception handler is GUARANTEED to return
;; one of 'abort, 'retry, or 'ignore. If the exception handler on the information returns
;; 'abort, then the enclosing abort-retry-ignore call should cease execution; you can program
;; its return value in this case to be anything. If the exception handler on the info returns
;; 'retry, then the abort-retry-ignore code should be run again (you don't have to worry about
;; if this will make an infinite loop). If it returns 'ignore, then execution should continue
;; as if interrupted was never called.
;;
;; This problem is difficult. Try to get just 'abort and 'ignore working -- I have made testcases
;; that only use those two. If you're feeling comfortable and have the time, implement 'retry.
;;
;; An example (you can uncomment this and move it below the definitions to do a basic test):
;;
;(define my-divisor 0)
;
;(define my-exception-handler
;  (lambda info (let ([problem (car info)])
;     (case problem
;       ['div-by-0 (begin (set! my-divisor 4) 'retry)] ; fix the problem then retry
;       ['big-number 'ignore] ; just ignore the problem
;       ['negative-number 'abort])))) ; kill the program entirely
;
;(abort-retry-ignore my-exception-handler
;    (lambda ()
;      (let ([mynum 200])
;        (if [eq? my-divisor 0] ; make sure we aren't dividing by 0
;            (interrupt 'div-by-0)
;            (set! mynum (/ mynum my-divisor)))
;        (displayln "Divided!")
;        (when [> mynum 30] ; is this number too big?
;            (interrupt 'big-number))
;        (set! mynum (- mynum 119))
;        (displayln "Subtracted!")
;        (when [negative? mynum] ; is the number negative?
;          (interrupt 'negative-number))
;        (displayln "Everything worked!")))) ; we shouldn't get to this line
;;
;; The expected output is "Divided!\nSubtracted!" but NOT "Everything worked!";
;; we should abort before executing that
;;
;; ASSUMPTIONS TO MAKE THINGS EASIER
;;
;; You may assume that the arguments passed to interrupt are compatible with
;; exception-handler. That is, if the thunk contains (interrupt 1 2) then
;; you can call (exception-handler 1 2) without fear.
;;
;; You may assume that abort-retry-ignore is never nested. That is, the thunk
;; will not call abort-retry-ignore.
;;
;; EXTRA PRACTICE
;;
;; 1) Implement 'retry. You may assume that the thunk will NEVER EVER return
;;    the symbol 'retried. If that assumption doesn't seem like it's useful,
;;    don't worry about it.
;;
;; 2) Add support for nested abort-retry-ignore calls, where the thunk does
;;    call abort-retry-ignore.

(define abort-retry-ignore
  (lambda (handler thunk)
    (nyi)))

(define interrupt
  (lambda info
    (nyi)))

;; QUESTION 3 interpreter
;; It's in final-exam-interpreter.rkt
;; This is to avoid variable name conflicts with A19's parser and expressions

;; QUESTION 4 macros (syntax-expand)

;; I'm giving you two problems. The first is something that can be only solved
;; with syntax-expand because of short-circuiting. The second is more of a "macro",
;; where the syntax expansion is basically a procedure but with a slightly more
;; usable syntax.

;; QUESTION 4.1 slist-member
;; Given an equality operator, a symbol, and an slist
;; (a possibly nested list of symbols),
;; determine whether that slist contains that symbol. Your solution
;; MUST short-circuit; that is, if you find the symbol early on, you
;; should not keep looking.
;;
;; Example: (slist-member eq? 'x ((w x y z) a b (c))) should return
;; #t, and never looks at z, a, b, or c.
;;
;; The grammar for an slist is
;; slist :: (s-expression*)
;; where
;; s-expression :: slist | symbol.
;;
;; Note I'm NOT quoting the slist!

(define-syntax (slist-member stx)
  (syntax-case stx ()
    [_ #'(nyi)]))

;; QUESTION 4.2 grade-tests
;; You will write a portion of the grading code that has been used in this
;; class to grade the assignments and exams. The syntax for this problem is
;;
;; (grade-tests testcase+)
;; where
;; testcase :: (e1 e2 pts) | (e1 e2 eq-op? pts).
;;
;; A testcase (e1 e2 pts) passes when e1 is equal? to e2.
;; A testcase (e1 e2 eq-op? pts) when e1 is eq-op? to e2.
;;
;; You should sum up all the points for the testcases that
;; passed and return the score of the assignment between
;; 0 and 1; divide the earned points by the total points.
;; For example:
;;
;; (grade-tests
;;    ('foo 'foo 3) ; passes
;;    ('bar 'baz 2) ; fails
;;    (4 6 (lambda (x y) (eq? (modulo x 2) (modulo y 2))) 4)) ; passes
;;
;; returns 7/9. Note that 7/9 is a number and not a string, it's just
;; Racket's fun way of formatting rationals.

(define-syntax (grade-tests stx)
  (syntax-case stx ()
    [_ #'(nyi)]))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
