#lang racket

;=====================================================================================================

(require "wires.rkt" "ternary-logic.rkt")

(provide
  agenda-make
  agenda?
  agenda-time
  current-agenda
  agenda-reset!
  agenda-empty?
  agenda-schedule!
  agenda-sequence!
  agenda-execute!
  agenda-time-limit
  agenda-report
  report-hidden
  report-time-width)

;=====================================================================================================

(struct agenda (name (timer #:mutable) hash)
  #:omit-define-syntaxes
  #:property prop:custom-write (λ (agnd port mode) (fprintf port "#<agenda:~s>" (agenda-name agnd)))
  #:property prop:object-name 0)

(define (agenda-make (name 'no-name))
  (unless (symbol? name) (raise-argument-error 'agenda-make "symbol?" name))
  (agenda name 0 (make-hash)))

(define (agenda-time) (agenda-timer (current-agenda)))
(define (agenda-empty?) (hash-empty? (agenda-hash (current-agenda))))

(define current-agenda
  (make-parameter (agenda-make 'the-agenda)
    (λ (agenda)
      (unless (agenda? agenda) (raise-argument-error 'current-agenda "agenda?" agenda))
      agenda)
    'current-agenda))

(define (agenda-reset!)
  (define agenda (current-agenda))
  (set-agenda-timer! agenda 0)
  (hash-clear! (agenda-hash agenda)))

(define (agenda-schedule! wire signal (delay 0))
  (unless (wire? wire) (raise-argument-error 'agenda-schedule! "wire?" wire))
  (unless (natural? delay) (raise-argument-error 'agenda-schedule! "natural?" delay))
  (unless (trit? signal) (raise-argument-error 'agenda-schedule! "trit?" signal))
  (define agenda (current-agenda))
  (define time (+ (agenda-time) delay))
  (define hash (agenda-hash agenda))
  (define events (hash-ref hash time '()))
  (define event (list wire signal))
  ; Do not schedule an event more than once for the same wire and time.
  (unless (member event events event-wire=?) (hash-set! hash time (cons event events))))

(define-syntax (agenda-sequence! stx)
  (syntax-case stx ()
    ((_ (wire (signal delay) ...) ...)
     #'(agenda-sequencer (list (list wire (list (list signal delay) ...)) ...)))))

(define (agenda-execute! (report (agenda-report)))
  (define (call action) (action))
  (define time-limit (agenda-time-limit))
  (define agenda (current-agenda))
  (define hash (agenda-hash agenda))
  (let loop ((time (agenda-time)))
    (unless (agenda-empty?)
      (when (and time-limit (> time time-limit))
        (error 'agenda-execute! "time-limit ~s exceeded" time-limit))
      (define events (sort (hash-ref hash time '()) event-wire-name<?))
      (hash-remove! hash time)
      ; An action is part of the gate to be triggered
      ; and therefore is the same (eq?) for inputs to the same gate.
      ; When two or more input wires of the same gate change signal,
      ; they activate the same gate related action.
      ; Therefore remove-duplicates can be used in order to prevent
      ; a gate from being triggered more than once at the same time.
      ; The order in which actions of distinct gates are executed is irrelevant.
      ; They are sorted by wire name.
      (define actions
        (remove-duplicates
          (for/fold ((actions '())) ((event (in-list events)))
            (define wire (car event))
            (define signal (cadr event))
            (cond
              ((eq? (wire-signal wire) signal) actions)
              (else (append (wire-signal-set! wire signal report) actions))))
          eq?))
      (hash-remove! hash time)
      (for-each call actions)
      (define new-time (add1 time))
      (set-agenda-timer! agenda new-time)
      (loop new-time))))

(define (wire-signal-set! wire signal report)
  (define old-signal (wire-signal wire))
  (unless (eq? signal old-signal)
    (when (and report (or (report-hidden) (not (hidden-wire? wire))))
      (printf "time ~a : ~a : ~s -> ~s~n"
        (~agenda-time)
        (wire-name* wire) old-signal signal))
    (set-wire-signal! wire signal)
    (wire-actions wire)))

(define agenda-time-limit
  (make-parameter 1000
    (λ (obj)
      (cond
        ((natural? obj) obj)
        ((not obj) #f)
        (else (raise-argument-error 'agenda-time-limit "(or/c #f natural?)" obj))))
    'agenda-time-limit))

(define agenda-report
  (make-parameter #f
    (λ (on/off) (and on/off #t))
    'agenda-report))

(define report-hidden
  (make-parameter #f
    (λ (obj) (and obj #t))
    'report-hidden))

(define report-time-width
  (make-parameter 1
    (λ (x) (unless (natural? x) (raise-argument-error 'report-time-width "natural?" x)) x)
    'report-time-width))

;=====================================================================================================

(define (agenda-sequencer lst)
  (cond
    ((null? lst) (void))
    (else
      (define wire (caar lst))
      (define signals/delays (cadar lst))
      (for ((signal/delay (in-list signals/delays)))
        (apply agenda-schedule! wire signal/delay))
      (agenda-sequencer (cdr lst)))))

(define (event-wire=? a b) (eq? (car a) (car b)))
(define (event-wire-name<? a b) (symbol<? (wire-name (car a)) (wire-name (car b))))

(define (~agenda-time) (~s #:min-width (report-time-width) #:align 'right (agenda-time)))
(define (wire-name* wire) (~s #:min-width (report-wire-width) #:align 'left (wire-name wire)))

;=====================================================================================================
; The end
