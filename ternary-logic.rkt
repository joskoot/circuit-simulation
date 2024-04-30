#lang racket

;=====================================================================================================

(provide
 F  T  ?
 F? T? ??
 trit? trits in-trits trit-case
 bit?  bits  in-bits  bit-case
 trit-combinations bit-combinations)

;=====================================================================================================

(struct trit ())
(struct bit trit ())

(define (make-trit-writer trit-symbol)
 (define (trit-writer trit port mode) (write trit-symbol port))
 trit-writer)

(struct F bit ()
 #:property prop:custom-write (make-trit-writer 'F)
 #:name trit-F
 #:constructor-name make-trit-F)

(struct T bit ()
 #:property prop:custom-write (make-trit-writer 'T)
 #:name trit-T
 #:constructor-name make-trit-T)

(struct ? trit ()
 #:property prop:custom-write (make-trit-writer '?)
 #:name trit-?
 #:constructor-name make-trit-?)

(define F (make-trit-F))
(define T (make-trit-T))
(define ? (make-trit-?))
(define trits (list F T ?))
(define bits  (list F T))
(define in-trits (in-list trits))
(define in-bits  (in-list bits))

(define (trit-select trit F-thunk T-thunk ?-thunk)
 (cond
  ((F? trit) (F-thunk))
  ((T? trit) (T-thunk))
  ((?? trit) (?-thunk))
  (else (raise-argument-error 'trit-select "trit?" trit))))

(define (bit-select bit F-thunk T-thunk)
 (cond
  ((F? bit) (F-thunk))
  ((T? bit) (T-thunk))
  (else (raise-argument-error 'bit-select "bit?" bit))))

(define-syntax (trit-case stx)
 (syntax-case stx ()
 ((_ trit (F-case ...) (T-case ...) (?-case ...))
#'(trit-select trit (thunk F-case ...) (thunk T-case ...) (thunk ?-case ...)))))

(define-syntax-rule (bit-case bit (F-case ...) (T-case ...))
 (bit-select bit (thunk F-case ...) (thunk T-case ...)))

(define-syntax (thunk stx)
 (syntax-case stx ()
  ((_) #'void)
  ((_ expr ...) #'(λ () expr ...))))

(define (trit-combinations n (sort? #f) #:vector (vector? #f))
 (unless (natural? n) (raise-argument-error 'trit-combinations "natural?" n))
 (define (trit-combinations n)
  (cond
   ((zero? n) '(()))
   (else
    (define trit-lists (trit-combinations (sub1 n)))
    (append
     (map cons-F trit-lists)
     (map cons-T trit-lists)
     (map cons-? trit-lists)))))
 (define trit-lists (trit-combinations n))
 (define combinations (if sort? (sort trit-lists trit-list<?) trit-lists))
 (if vector? (list->vector combinations) combinations))

(define (bit-combinations n #:vector (vector? #f))
 (unless (natural? n) (raise-argument-error 'list-lists-of-trits "natural?" n))
 (define (bit-combinations n)
  (cond
   ((zero? n) '(()))
   (else
    (define bit-lists (bit-combinations (sub1 n)))
    (append
     (map cons-F bit-lists)
     (map cons-T bit-lists)))))
 (define combinations (bit-combinations n))
 (if vector? (list->vector combinations) combinations))

(define (cons-F list-of-trits) (cons F list-of-trits))
(define (cons-T list-of-trits) (cons T list-of-trits))
(define (cons-? list-of-trits) (cons ? list-of-trits))

(define (trit-list<? trit-list-x trit-list-y)
 (< (count ?? trit-list-x) (count ?? trit-list-y)))
        
;=====================================================================================================
; The end
