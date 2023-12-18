
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

(provide (all-defined-out))
