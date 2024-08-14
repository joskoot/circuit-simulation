#lang racket

;=====================================================================================================

(require "ternary-logic.rkt")

(provide
  wire-make
  wire?
  wire-name
  wire-signal
  wire-init-signal
  define-wires
  report-wire-width
  wire-print
  wire-println
  wire-nr-of-events)

(provide ; for private use only
  set-wire-signal!
  wire-events
  wire-add-event!
  hidden-wire?)

;=====================================================================================================

(struct wire (name (signal #:mutable) (events #:mutable))
  
  #:auto-value 0
  #:property prop:custom-write
  (λ (wire port mode) (fprintf port "#<wire:~s=~s>" (wire-name wire) (wire-signal wire)))
  #:property prop:object-name 0)

(struct hidden-wire wire () #:omit-define-syntaxes)

(define (wire-make name (signal (wire-init-signal)))
  (unless (symbol? name) (raise-argument-error 'wire-make "symbol?" name))
  (unless (trit? signal) (raise-argument-error 'wire-make "trit?" signal))
  ((if (to-be-hidden? name) hidden-wire wire) name signal '()))

(define (to-be-hidden? name) (regexp-match? #px".*•[0 1 2 3 4 5 6 7 8 9]+$" (symbol->string name)))

(define wire-init-signal
  (make-parameter ?
    (λ (signal)
      (unless (trit? signal) (raise-argument-error 'wire-init-signal "trit?" signal))
      signal)
    'wire-init-signal))

(define (wire-add-event! wire event)
  (define events (wire-events wire))
  ; Avoid duplicate events.
  (unless (member event events)
    (set-wire-events! wire (cons event events))))

(define-syntax (define-wires stx)
  (syntax-case stx ()
    ((_ decl ...)
     (with-syntax
       ((((id expr) ...)
         (for/list ((d (in-list (syntax->list #'(decl ...)))))
           (syntax-case d ()
             (id (identifier? #'id) #'(id (wire-make 'id)))
             ((id) (identifier? #'id) #'(id (wire-make 'id)))
             ((id name/signal) (identifier? #'id) #'(id (wire-maker 'id name/signal)))
             ((id name signal) (identifier? #'id) #'(id (wire-make name signal)))
             (else (raise-syntax-error 'define-wires "(identifier? [expr expr])" stx d))))))
       #'(define-values (id ...) (values expr ...))))))

(define (wire-maker name name/signal)
  (if (symbol? name/signal)
    (wire-make name/signal)
    (wire-make name name/signal)))

(define report-wire-width
  (make-parameter 1
    (λ (x) (unless (natural? x) (raise-argument-error 'report-wire-width "natural?" x)) x)
    'report-wire-width))

(define (wire-print #:port (p 'current) . wires)
  (cond
    ((null? wires) (when (eq? p 'string) ""))
    (else
      (define port
        (cond
          ((eq? p 'current) (current-output-port))
          ((eq? p 'string) (open-output-string))
          ((output-port? p) p)
          (else (raise-argument-error 'wire-print "(or/c 'current 'string port?)" p))))
      (parameterize ((current-output-port port))
        (for/fold ((start #t)) ((wire (in-list wires)))
          (unless (wire? wire) (raise-argument-error 'wire-print "wire?" wire))
          (if start
            (printf "~a = ~s"
              (~s #:min-width (report-wire-width) (wire-name wire))
              (wire-signal wire))
            (printf "~n~a = ~s"
              (~s #:min-width (report-wire-width) (wire-name wire))
              (wire-signal wire)))
          #f))
      (cond
        ((not (eq? p 'string)) (newline port))
        ((eq? p 'string) (get-output-string port))))))

(define (wire-println #:port (p 'current) #:sep (sep " ") . wires)
  (cond
    ((null? wires) (when (eq? p 'string) ""))
    (else
      (define port
        (cond
          ((eq? p 'current) (current-output-port))
          ((eq? p 'string) (open-output-string))
          ((output-port? p) p)
          (else (raise-argument-error 'wire-print "(or/c 'current 'string port?)" p))))
      (parameterize ((current-output-port port))
        (for/fold ((start #t)) ((wire (in-list wires)))
          (unless (wire? wire) (raise-argument-error 'wire-println "wire?" wire))
          (if start
            (printf  "~a=~s" (wire-name wire) (wire-signal wire))
            (printf "~a~a=~s" sep (wire-name wire) (wire-signal wire)))
          #f))
      (cond
        ((not (eq? p 'string)) (newline port))
        ((eq? p 'string) (get-output-string port))))))

(define (wire-nr-of-events wire)
  (unless (wire? wire) (raise-argument-error 'wire-nr-of-events "wire?" wire))
  (length (wire-events wire)))

;=====================================================================================================
; The end
