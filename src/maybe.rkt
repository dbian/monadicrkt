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