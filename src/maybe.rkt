#lang racket
(provide (all-defined-out))

(struct maybe-type-just [value])
(struct maybe-type-nothing [])

(define (make-just value)
  (maybe-type-just value))

(define (make-nothing)
  (maybe-type-nothing))

(define (maybe-type? x)
  (or (maybe-type-just? x)
      (maybe-type-nothing? x)))

(define (maybe-map maybe f)
  (cond ((maybe-type-just? maybe)
         (make-just (f (maybe-type-just-value maybe))))
        ((maybe-type-nothing? maybe)
         maybe)))

(define (maybe-bind maybe f)
  (cond ((maybe-type-just? maybe)
         (f (maybe-type-just-value maybe)))
        ((maybe-type-nothing? maybe)
         maybe)))

(define (maybe-just-value maybe)
  (maybe-type-just-value maybe))

(define-syntax maybe/do
  (syntax-rules (<-)
    ((_ (val1 <- maybe1) exp1 exp2 ...)
     (maybe-bind maybe1
                 (λ (val1) (maybe/do exp1 exp2 ...))))
    ((_ exp1 exp2 exp3 ...)
     (maybe-bind exp1 (λ (_) (maybe/do exp2 exp3 ...)))
     )
    ((_ exp1) exp1
              )
    ))