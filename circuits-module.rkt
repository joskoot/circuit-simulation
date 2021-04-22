#lang racket

; Bu Jacob J. A. Koot

(require "wires.rkt" "agenda.rkt" "ternary-logic.rkt" "logical-functions.rkt" (for-syntax racket))

(provide
 (all-from-out "wires.rkt" "ternary-logic.rkt" "logical-functions.rkt" "agenda.rkt")
 make-circuit-constr make-gate*-constr
 circuit-constr? circuit-constr-name
 reset-hidden-cntr!
 Not         Not-function
 And And3    And-function
 Nand Nand3  Nand-function
 Or Or3      Or-function
 Nor Nor3    Nor-function
 Xor         Xor-function
 And* Nand* Or* Nor* Xor*   
 Imply       Imply-function
 If          If-function
 Delay)

;=====================================================================================================

(struct circuit-constr (name proc)
 #:property prop:procedure 1
 #:property prop:custom-write
 (λ (circuit port mode) (fprintf port "#<circuit-constr:~s>" (circuit-constr-name circuit)))
 #:property prop:object-name (λ (x) (circuit-constr-name x))
 #:guard
 (λ (name proc ignore)
  (unless (procedure? proc) (raise-argument-error 'circuit-constr "procedure?" proc))
  (unless (symbol? name) (raise-argument-error 'circuit-constr "symbol?" name))
  (values name proc)))

(define-syntax (make-circuit-constr stx)
 (syntax-case stx ()
  ((_ name (in ...) (out ...) (sub-out (sub-circuit-constr sub-in ...)) ...)
   (with-syntax
    ((((sub-out ...) ...)
      (map (λ (x) (if (identifier? x) (list x) x)) (syntax->list #'(sub-out ...)))))
    (cond
     ((andmap identifier? (syntax->list #'(sub-in ... ...)))
    #'(basic-make-circuit-constr name (in ...) (out ...)
       ((sub-out ...) (sub-circuit-constr sub-in ...)) ...))
     (else
    #`(make-circuit-constr name (in ...) (out ...)
    #,@(apply append
        (map parse-subcircuit
         (syntax->list #'(((sub-out ...) (sub-circuit-constr sub-in ...)) ...)))))))))))

(define-syntax (basic-make-circuit-constr stx)
 (syntax-case stx (define)
  ((_ name (in ...) (out ...) ((sub-out ...) (sub-circuit-constr sub-in ...)) ...)
   (check-args stx
    (syntax->list #'(in ...))
    (syntax->list #'(out ...))
    (map syntax->list (syntax->list #'((sub-in ...) ...)))
    (map syntax->list (syntax->list #'((sub-out ...) ...))))
   (with-syntax
    (((sub-c ...) (generate-temporaries #'(sub-circuit-constr ...)))
     ((internal-wire ...)
      (remove*
       (remove-duplicates
        (syntax->list #'(in ... out ...))
        bound-identifier=?)
       (remove-duplicates
        (syntax->list #'(sub-out ... ... sub-in ... ...))
        bound-identifier=?)
       bound-identifier=?))
     ((arg ...) (remove-duplicates (syntax->list #'(in ... out ...)) bound-identifier=?)))
  #'(let ((neem name))
     (unless (symbol? neem)
      (raise-argument-error 'circuit-constr "symbol for argument name" neem))
     (circuit-constr neem
      (let ((bussy (make-parameter #f)))
       (λ (arg ...)
        (when (bussy)
         (error neem "cannot be a subcircuit of itself,\n  nor directly, nor indirectly"))
        (parameterize ((bussy #t))
         (let ((bussy (make-parameter #f)))
          (unless (circuit-constr? sub-circuit-constr)
           (error 'make-circuit-constr
            "circuit-constr expected, given ~s" sub-circuit-constr)) ...
          (let ((sub-c sub-circuit-constr) ...)
           (define internal-wire (make-hidden-wire 'internal-wire)) ...
           (when (bussy) (error 'neem "infinitely nested circuit"))
           (parameterize ((bussy #t))
            (sub-c sub-in ... sub-out ...) ...
            (void)))))))))))))

(define-for-syntax (parse-subcircuit stx)
 (define (extract x) (define k (car (syntax->datum x))) (add-dot (if (symbol? k) k (car k))))
 (define (add-dot smbl) (string->symbol (format "~s•" smbl)))
 (syntax-case stx ()
  (((sub-out ...) (sub-circuit-constr sub-in ...))
   (cond
    ((andmap identifier? (syntax->list #'(sub-in ...))) (list stx))
    (else
     (define non-ids (filter (compose not identifier?) (syntax->list #'(sub-in ...))))
     (define temps (generate-temporaries (map extract non-ids)))
     (cons
    #`((sub-out ...) (sub-circuit-constr #,@(rename-hidden temps (syntax->list #'(sub-in ...)))))
      (generate-gates temps non-ids)))))))

(define (make-hidden-wire wire-name)
 (set! hidden-counter (add1 hidden-counter))
 (wire-make
  (string->symbol
   (regexp-replace
    #px"•[0 1 2 3 4 5 6 7 8 9]+$"
    (symbol->string wire-name)
    (format "•~s" hidden-counter)))))

(define-for-syntax (rename-hidden temps sub-ins)
 (cond
  ((null? sub-ins) '())
  ((identifier? (car sub-ins)) #`(#,(car sub-ins) #,@(rename-hidden temps (cdr sub-ins))))
  (else #`(#,(car temps) #,@(rename-hidden (cdr temps) (cdr sub-ins))))))

(define hidden-counter 0)
(define (reset-hidden-cntr!) (set! hidden-counter 0))

(define-for-syntax (generate-gates temps non-ids)
 (cond
  ((null? temps) '())
  (else (cons #`((#,(car temps)) #,(car non-ids)) (generate-gates (cdr temps) (cdr non-ids))))))

(define-for-syntax (check-args stx in out sub-ins sub-outs)
 (define (check-ids ids)
  (cond
   ((null? ids))
   ((identifier? (car ids)) (check-ids (cdr ids)))
   (else (raise-syntax-error 'make-syntax-constr "identifier expected" stx (car ids)))))
 (define (check-dup ids)
  (define id (check-duplicate-identifier ids))
  (when id (raise-syntax-error 'make-syntax-constr "duplicate identifier" stx id)))
 (define (check-subset a b msg)
  (cond
   ((null? a))
   ((member (car a) b bound-identifier=?) (check-subset (cdr a) b msg))
   (else (raise-syntax-error 'make-circuit-constr msg stx (car a)))))
 (define all-sub-ins (filter identifier? (apply append sub-ins)))
 (define all-sub-outs (apply append sub-outs))
 (check-ids in)
 (check-ids out)
 (check-ids all-sub-ins)
 (check-ids all-sub-outs)
 (check-dup in)
 (check-dup out)
 ;(for-each check-dup sub-ins)
 (check-dup all-sub-outs)
 (check-subset in (append all-sub-ins out) "every input must be subcircuit input or an output")
 (check-subset out (append all-sub-outs in) "every output must be a subcircuit output or an input")
 (check-subset all-sub-ins (append in all-sub-outs)
               "Every subcircuit input must be an input or a subcircuit output")
 (check-subset all-sub-outs (append out all-sub-ins)
               "Every subcircuit output must be an output and/or a subcircuit input."))

(define (make-gate*-constr name function)
 (procedure-rename
  (λ (delay)
   (circuit-constr name
    (λ wires
     (define ws (reverse wires))
     (define input-wires (cdr ws))
     (define output-wire (car ws))
     (define (action)
      (define output-signal (apply function (map wire-signal input-wires)))
      (unless (eq? (wire-signal output-wire) output-signal)
       (agenda-schedule! output-wire output-signal delay)))
     (for ((input-wire (in-list input-wires))) (wire-add-action! input-wire action)))))
  (symbol-append name '-constr)))

(define-syntax (function->gate-constr stx)
 (syntax-case stx ()
  ((_ name (input-wire ...) ((output-wire delay)) logical-function)
 #'(let ((proc logical-function) (neem name))
    (circuit-constr neem
     (λ (input-wire ... output-wire)
      (define (action)
       (define signal (proc (wire-signal input-wire) ...))
       (unless (eq? signal (wire-signal output-wire))
        (agenda-schedule! output-wire signal delay)))
      (wire-add-action! input-wire action) ...))))))

;=====================================================================================================

(define Not   (function->gate-constr 'Not   (in)    ((out 1)) Not-function))
(define Nand  (function->gate-constr 'Nand  (a b)   ((out 1)) Nand-function))
(define Nand3 (function->gate-constr 'Nand3 (a b c) ((out 1)) Nand-function))

(define And
 (make-circuit-constr 'And (a b) (out)
  ((out) (Not (Nand a b)))))

(define And3
 (make-circuit-constr 'And3 (a b c) (out)
  ((out) (Not (Nand3 a b c)))))

(define Or
 (make-circuit-constr 'Or (a b) (out)
  ((out) (Nand (Not a) (Not b)))))

(define Or3
 (make-circuit-constr 'Or3 (a b c) (out)
 ((out) (Nand3 (Not a) (Not b) (Not c)))))

(define Nor
 (make-circuit-constr 'Nor (a b) (out)
  ((out) (And (Not a) (Not b)))))

(define Nor3
 (make-circuit-constr 'Nor3 (a b c) (out)
 ((out) (And3 (Not a) (Not b) (Not c)))))

(define Xor
 (make-circuit-constr 'Xor (a b) (out)
  ((out) (Nand (Nand a (Not b)) (Nand (Not a) b)))))

(define If
 (make-circuit-constr 'If (test then else) (out)
  ((out) (Nand3 (Nand then else) (Nand test then) (Nand (Not test) else)))))

(define Imply
 (make-circuit-constr 'Imply (premise implication) (out)
  ((out) (Nand premise (Not implication)))))

(define (Delay delta)
 (unless (exact-positive-integer? delta)
  (raise-argument-error 'Delay "exact-positive-integer?" delta))
 (function->gate-constr 'Delay (a) ((b delta)) values))

(define (symbol-append . symbols)
 (string->symbol (apply string-append (map symbol->string symbols))))

(define And*  (make-gate*-constr 'And*   And-function))
(define Nand* (make-gate*-constr 'Nand* Nand-function))
(define Or*   (make-gate*-constr 'Or*     Or-function))
(define Nor*  (make-gate*-constr 'Nor*   Nor-function))
(define Xor*  (make-gate*-constr 'Xor*   Xor-function))

;=====================================================================================================
; The end
