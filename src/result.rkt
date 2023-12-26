
#lang racket
(require "maybe.rkt")
(struct result-type-success [value])
(struct result-type-error [message])

(define (make-success value)
  (result-type-success value))

(define (make-error message)
  (result-type-error message))

(define (result-type? x)
  (or (result-type-success? x)
      (result-type-error? x)))

(define (result-map result f)
  (cond ((result-type-success? result)
         (make-success (f (result-type-success-value result))))
        ((result-type-error? result)
         result)))

(define (result-bind result f)
  (cond ((result-type-success? result)
         (f (result-type-success-value result)))
        ((result-type-error? result)
         result)))

(define (result2maybe result)
  (cond ((result-type-success? result)
         (make-just (result-type-success-value result)))
        ((result-type-error? result)
         (make-nothing))))

(define (result-success-val result)
  (result-type-success-value result))

(define (result-error-val result)
  (result-type-error-message result))

(define-syntax result/do
  (syntax-rules (<-)
    ((_ (val1 <- result1) exp1 exp2 ...)
     (result-bind result1
                  (λ (val1) (result/do exp1 exp2 ...))))
    ((_ exp1 exp2 exp3 ...)
     (result-bind exp1 (λ (_) (result/do exp2 exp3 ...)))
     )
    ((_ exp1) exp1
              )
    ))

(provide (all-defined-out))
