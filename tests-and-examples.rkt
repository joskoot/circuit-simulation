#lang racket

(require "circuits.rkt")

;=====================================================================================================

(define line (make-string 100 #\-))
(define (border) (printf "\n~a\n\n" line))
(border)
(printf "Examples including tests.~n")
(border)

;=====================================================================================================
; Gates: tests and measurement of delays

(printf "Gates: tests and measurement of delays~n~n")

(define-syntax (check-gate stx)
 (syntax-case stx ()
  ((_ gate (in ...) (expected ...))
 #'(let
    ((computed
      (for*/list ((in in-trits) ...)
       (agenda-reset!)
       (let
        ((in (wire-make 'x in)) ... (out (wire-make 'out)))
        (gate in ... out)
        (parameterize ((agenda-report #f)) (agenda-execute!))
        (print-results gate (in ...) (out))
        (wire-signal out)))))
    (unless (equal? computed (list expected ...))
     (printf "~s~n" computed)
     (error (circuit-constr-name gate) "test fails"))
    (newline)))))

(define-syntax (print-results stx)
 (syntax-case stx ()
  ((_ name (in ...) (out ...))
 #'(begin
    (printf "~s :" 'name)
    (printf " ~s=~s" 'in (get-signal in)) ...
    (printf " -->")
    (printf " ~s=~s" 'out (get-signal out)) ...
    (printf " time=~s~n" (max 0 (sub1 (agenda-time))))))))

(define (get-signal signal-or-wire)
 (if (wire? signal-or-wire) (wire-signal signal-or-wire) signal-or-wire))

(check-gate Not   (a  ) (T F ?))
(check-gate And   (a b) (F F F F T ? F ? ?))
(check-gate Nand  (a b) (T T T T F ? T ? ?))
(check-gate Or    (a b) (F T ? T T T ? T ?))
(check-gate Nor   (a b) (T F ? F F F ? F ?))
(check-gate Xor   (a b) (F T ? T F ? ? ? ?))
(check-gate Imply (a b) (T T T F T ? ? T ?))
(check-gate If    (test then else) (F T ? F T ? F T ? F F F T T T ? ? ? F ? ? ? T ? ? ? ?))
(check-gate Nand3 (a b c) (T T T T T T T T T T T T T F ? T ? ? T T T T ? ? T ? ?))
(check-gate Or3   (a b c) (F T ? T T T ? T ? T T T T T T T T T ? T ? T T T ? T ?))
(check-gate Nor3  (a b c) (T F ? F F F ? F ? F F F F F F F F F ? F ? F F F ? F ?))
(agenda-reset!)

(printf "~a~n~n" line)

;=====================================================================================================
; Delay

(printf "Delay~n~n")
(agenda-reset!)
(random-seed 1)

(parameterize ((agenda-report #t) (report-time-width 2))
 (define p (wire-make 'p))
 (define s (wire-make 's))
 (define Delay10 (Delay 10))
 (define str-port (open-output-string))
 (parameterize ((current-output-port str-port))
  ((make-circuit-constr 'delay (p) (s)
     ((q) (Delay10 p))
     ((r) (Not q))
     ((s) ((Delay 5) r)))
   p s)
  (agenda-schedule! p T)
  (agenda-execute! #t))
 (define str (get-output-string str-port))
 (display str)
 (unless (equal? str (string-append "time  0 : p : ? -> T\n"
                                    "time 10 : q : ? -> T\n"
                                    "time 11 : r : ? -> F\n"
                                    "time 16 : s : ? -> F\n"))
  (error "delay test failed")))

(border)

;=====================================================================================================
; Adder

(define make-half-adder
 (make-circuit-constr 'make-half-adder(a b) (s c)
  ((d) (Or a b))
  ((c) (And a b))
  ((e) (Not c))
  ((s) (And d e))))

(define make-full-adder
 (make-circuit-constr 'make-full-adder (a b c-in) (sum c-out)
 ((s c1) (make-half-adder b c-in))
 ((sum c2)(make-half-adder a s))
 ((c-out) (Or c1 c2))))

(for* ((a in-bits) (b in-bits) (c in-bits))
 (define wa (wire-make 'a a))
 (define wb (wire-make 'b b))
 (define wc (wire-make 'c c))
 (define sum (wire-make 'sum))
 (define cout (wire-make 'cout))
 (make-full-adder  wa wb wc sum cout)
 (parameterize ((agenda-report #f)) (agenda-execute! #f))
 (printf "full-adder a=~s, b=~s, carry-in=~s --> sum=~s, carry-out=~s time=~s~n"
  a b c (wire-signal sum) (wire-signal cout) (max 0 (sub1 (agenda-time))))
 (agenda-reset!)
 (define result (list (wire-signal sum) (wire-signal cout)))
 (unless
  (case (count T? (list a b c))
   ((0) (equal? result (list F F)))
   ((1) (equal? result (list T F)))
   ((2) (equal? result (list F T)))
   ((3) (equal? result (list T T))))
  (error 'full-adder "~s" (list a b c result))))

; Two's complement 6-bit adder:

(define (make-6-wires name)
 (apply values
  (build-list 6
   (λ (i) (wire-make (string->symbol (format "~s~s" name i)))))))

(define-values (a0 a1 a2 a3 a4 a5) (make-6-wires 'a))
(define-values (b0 b1 b2 b3 b4 b5) (make-6-wires 'b))
(define-values (s0 s1 s2 s3 s4 s5) (make-6-wires 's))
(define carry-in (wire-make 'carry-in))
(define carry-out (wire-make 'carry-out))
(define overflow (wire-make 'overflow))

(define make-6-bit-adder
 (make-circuit-constr '6-bit-adder
  (a5 a4 a3 a2 a1 a0
   b5 b4 b3 b2 b1 b0
   carry-in)
  (s5 s4 s3 s2 s1 s0 carry-out overflow)
  ((s0 c1)        (make-full-adder a0 b0 carry-in))
  ((s1 c2)        (make-full-adder a1 b1 c1))
  ((s2 c3)        (make-full-adder a2 b2 c2))
  ((s3 c4)        (make-full-adder a3 b3 c3))
  ((s4 c5)        (make-full-adder a4 b4 c4))
  ((s5 carry-out) (make-full-adder a5 b5 c5))
  ((overflow)     (Xor c5 carry-out))))

(make-6-bit-adder a5 a4 a3 a2 a1 a0 b5 b4 b3 b2 b1 b0 carry-in s5 s4 s3 s2 s1 s0 carry-out overflow)

(define (n->b n)
 (for/fold ((mask 1) (b '()) #:result b) ((i (in-range 6)))
  (values
   (* 2 mask)
   (cons (if (zero? (bitwise-and mask n)) F T) b))))

(define (b->n b)
 (for/fold ((n 0) (k 1) #:result (if (> n 31) (- n 64) n)) ((bit (in-list (reverse b))))
  (values
   (if (F? bit) n (+ n k))
   (* 2 k))))

(printf "~nTest 6-bit-adder:~n")

(unless
 (for*/and ((n (in-range -32 32)) (m (in-range -32 32)))
  (agenda-reset!)
  (define a (n->b n))
  (define b (n->b m))
  (for ((a (in-list (list a5 a4 a3 a2 a1 a0)))
        (bit (in-list (n->b n))))
       (agenda-schedule! a bit))
  (for ((b (in-list (list b5 b4 b3 b2 b1 b0)))
        (bit (in-list (n->b m))))
       (agenda-schedule! b bit))
  (agenda-schedule! carry-in F)
  (agenda-execute!)
  (cond
   ((> (+ n m)  31) (T? (wire-signal overflow)))
   ((< (+ n m) -32) (T? (wire-signal overflow)))
   (else (= (b->n (map wire-signal (list s5 s4 s3 s2 s1 s0))) (+ n m)))))
 (error "test failed"))
(displayln 'ok)
(agenda-reset!)
(border)

;=====================================================================================================
; D-flip-flop

(parameterize ((agenda-time-limit 200))
 (printf "D-flip-flop~n~n~
          Test future signal T on clock at time 30,~n~
          starting up with clock=0~n~
          causing unstability until clock raises to T.~n~n")
 (define (printer old-state old-state-inverse)
  (print-results D-flip-flop
   (in clock old-state old-state-inverse)
   (state state-inverse)))
 (define in (wire-make 'in T))
 (define clock (wire-make 'clock F))
 (define state (wire-make 'state F))
 (define state-inverse (wire-make 'state-inverse F))
 (define D-flip-flop
  (make-circuit-constr 'D-flip-flop (in clock) (state state-inverse)
   ((reset)         (Nand clock in))
   ((set)           (Nand clock reset))
   ((state)         (Nand reset state-inverse))
   ((state-inverse) (Nand set   state))))
 (D-flip-flop in clock state state-inverse)
 ; Schedule a T pulse one the clock for time 30 to time 60.
 (agenda-schedule! clock T 30)
 (agenda-schedule! clock F 60)
 (parameterize ((agenda-report #t) (report-wire-width 13)) (agenda-execute! #t))
 (printf "~nD-flip-flop stabilized: ~a time=~s~n"
  (wire-println #:port 'string in clock state state-inverse) (max 0 (sub1 (agenda-time))))
 (printf "Notice the vibration of state and state-inverse from time=2 ~n~
          up to somewhat later than time=30 at which moment the clock is raised.~n~
          Vibration of state halts at time=32 and that of state-inverse at time=33.~n")
 (unless
  (and
   (T? (wire-signal state))
   (F? (wire-signal state-inverse)))
  (error 'D-flip-flop "stabilizing failed"))
 (printf "Now that the D-flip-flop is stable ~
          we can test all combinations of in, clock and old-state.~n~n")
 (agenda-reset!)
 (define (do i c s) ; i=in, c=clock, s=old state
  (agenda-reset!)
  ; Put D-flip-flop in state=i with a T pulse on the clock.
  (agenda-schedule! clock T)
  (agenda-schedule! in s)
  (agenda-execute!)
  (agenda-schedule! clock F)
  (agenda-execute!)
  ;(agenda-reset!)
  (check i F s (wire-signal state) (wire-signal state-inverse))
  ; Test with in=i, clock=c and old-state=s.
  (define old-state (wire-signal state))
  (define old-state-inverse (wire-signal state-inverse))
  (agenda-schedule! in i)
  (agenda-schedule! clock c)
  (agenda-execute!)
  (printer old-state old-state-inverse)
  ;(agenda-reset!)
  (check i c s (wire-signal state) (wire-signal state-inverse)))
 (define (check i c s state not-state)
  (unless
   (cond
    ((F? c) (and (eq? state s) (eq? not-state (Not-function s))))
    (else (and (eq? state i) (eq? not-state (Not-function i)))))
   (error 'D-flip-flop "test failed ~s" (list i c s state not-state))))
 (for* ((i in-bits) (c in-bits) (s in-bits)) (do i c s)))

;=====================================================================================================
; Twin-flip-flop (usually called master-slave-flip-flop)
; The twin-flip-flop consists of two SR-latches.

(border)
(printf "Twin-flip-flop~n~n")

(define SR-latch
 (make-circuit-constr 'SR-latch
  (S R clock) 
  (P Q)       
  ((reset) (Nand S clock))
  ((set)   (Nand R clock))
  ((P)     (Nand reset Q))
  ((Q)     (Nand set   P))))

(define twin-flip-flop
 (make-circuit-constr 'twin-flip-flop
  (J K clock) 
  (P Q)
  ; When both J and K are 1,
  ; the second SR-latch must flip the first one.
  ; In this case P and Q must be fed back
  ; to inputs S1 and R1 of the first SR-latch.
  ((a) (Nand K P))
  ((b) (Nand J Q))
  ((not-clock) (Not clock))
  ((S1) (And J a))
  ((R1) (And K b))
  ; The two SR-latches.
  ((P1 Q1) (SR-latch S1 R1     clock))
  ((P  Q ) (SR-latch P1 Q1 not-clock))))

(define J     (wire-make 'J))
(define K     (wire-make 'K))
(define clock (wire-make 'clock))
(define P     (wire-make 'P))
(define Q     (wire-make 'Q))

(twin-flip-flop J K clock P Q)

; Now we can test for all J, K and old-state

(define (check signal)
 (unless
  (and
   (equal? (wire-signal P) signal)
   (equal? (wire-signal Q) (Not-function signal)))
  (error 'twin-flip-flop "test failed")))

(for* ((j in-bits) (k in-bits) (old-state in-bits))
 (agenda-reset!)
 ; First put the twin-flip-flop in state=old-state.
 (agenda-schedule! J old-state)
 (agenda-schedule! K (Not-function old-state))
 (agenda-schedule! clock T)
 (agenda-execute!)
 (agenda-schedule! clock F)
 (agenda-execute!)
 ; Check that the twin-flip-flop is initialized correctly.
 (check old-state)
 ; Now clock the twin-flip-flop with J=j and K=k.
 (agenda-schedule! J j)
 (agenda-schedule! K k)
 (agenda-schedule! clock T)
 (agenda-execute!)
 (agenda-reset!)
 (agenda-schedule! clock F)
 (agenda-execute!)
 ; Check the results.
 (check
  (case (format "~s ~s ~s" j k old-state)
   (("F F F") F)
   (("T F F") T)
   (("F T F") F)
   (("T T F") T)
   (("F F T") T)
   (("T F T") T)
   (("F T T") F)
   (("T T T") F)))
 (printf "J=~s, K=~s, old-state=~s --> state=~s, state-inverse=~s, time=~s~n"
  j k old-state (wire-signal P) (wire-signal Q) (max 0 (sub1 (agenda-time)))))

;=====================================================================================================

;(border)
;(display "Starred gates\n")
;(define m 0)
;
;(define-syntax (runner stx)
; (syntax-case stx ()
;  ((_ gate in ...)
;   (with-syntax (((w ...) (generate-temporaries #'(in ...))))
;  #'(begin
;     (set! m 0)
;     (newline)
;     (for* ((in in-trits) ...)
;      (set! m (add1 m))
;      (agenda-reset!)
;      (define out (wire-make 'out))
;      (define w (wire-make 'in in)) ...
;      ((gate m) w ... out)
;      (agenda-execute!)
;      (printf "~a " (~s #:min-width 2 #:align 'right m))
;      (wire-println w ... out)))))))
;
;(define-syntax (run stx)
; (syntax-case stx ()
;  ((_ gate name)
; #'(let ()
;    (printf "~ngate ~s~n" name)
;    (runner gate)
;    (runner gate a)
;    (runner gate a b)
;    (runner gate a b c)
;    (runner gate a b c d)))))
;
;(for ((gate (in-list (list And* Nand* Or* Nor* Xor*)))
;      (name (in-list '(    And* Nand* Or* Nor* Xor*)))) (run gate name))

;=====================================================================================================

(border)
(printf "The end~n")

