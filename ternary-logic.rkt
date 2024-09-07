#lang racket/base

(require (only-in racket natural? count))

(provide
  F T ? F? T? ??
  trit? bit?
  trits bits
  in-trits in-bits
  trit-case bit-case
  trit-combinations bit-combinations)

(define-syntax-rule (define-bit/trit bit/trit name pred)
  (define-values (name pred)
    (let ()
      (struct name bit/trit ()
        #:omit-define-syntaxes
        #:property prop:object-name (λ (ignore) 'name)
        #:property prop:custom-write (λ (o p m) (write 'name p)))
      (values (name) pred))))

(struct trit ())
(struct bit trit ())
(define-bit/trit bit F F?)
(define-bit/trit bit T T?)
(define-bit/trit trit ? ??)
(define trits (list F T ?))
(define bits (list F T))
(define in-trits (in-list trits))
(define in-bits (in-list bits))

(define-syntax-rule (trit-case trit (F-body ...) (T-body ...) (?-body ...))
  (let ((t trit))
    (cond
      ((F? t) F-body ...)
      ((T? t) T-body ...)
      ((?? t) ?-body ...)
      (else (raise-argument-error 'trit-case "trit?" t)))))

(define-syntax-rule (bit-case trit (F-body ...) (T-body ...))
  (let ((t trit))
    (cond
      ((F? t) F-body ...)
      ((T? t) T-body ...)
      (else (raise-argument-error 'bit-case "bit?" t)))))

(define (trit-combinations n #:sort (sort? #f) #:vector (vector? #f))
  (unless (natural? n) (raise-argument-error 'trit-combinations "natural?" n))
  (define combis (combinations trits n))
  (case (list (and sort? #t) (and vector? #t))
    (((#f #f)) combis)
    (((#f #t)) (apply vector combis))
    (((#t #f)) (sort combis combination<?))
    (((#t #t)) (apply vector (sort combis combination<?)))))

(define (bit-combinations n #:vector (vector? #f))
  (unless (natural? n) (raise-argument-error 'bit-combinations "natural?" n))
  (define combis (combinations bits n))
  (cond
    (vector? (apply vector combis))
    (else combis)))

(define (combinations lst n)
  (cond
    ((zero? n) '(()))
    ((null? lst) '())
    (else
      (define in-lst (in-list lst))
      (define (combinations n)
        (cond
          ((zero? n) (map list lst))
          (else
            (define in-combis (in-list (combinations (sub1 n))))
            (for*/list ((e in-lst) (combi in-combis)) (cons e combi)))))
      (combinations (sub1 n)))))

(define (combination<? a b) (< (count ?? a) (count ?? b)))
