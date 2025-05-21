#lang racket/base

(require circuit-simulation/circuits)

(random-seed 0)

(define-wires (a ?) (b ?) (c ?) (d ?))
(define signals (vector F T ?))
(define (rs) (vector-ref signals (random 3)))

(define Probe
  ((make-gate*-constr 'Print
     (λ signals
       (printf "Probe: time ~s, (a b c) = ~s~n" (agenda-time) signals)
       ?))
   1))

(define probed (make-circuit-constr 'y (a b c) (d)  ((d) (Probe a b c))))

(for ((t (in-range 1 10)))
  (let loop ()
    (define-values (sa sb sc) (values (rs) (rs) (rs)))
    (cond
      ((equal? (list sa sb sc) (map wire-signal (list a b c)))
       (printf "skipped ~s~n" (list sa sb sc))
       (loop))
      (else
        (agenda-sequence! (a (sa t)) (b (sb t)) (c (sc t)))))))

(probed a b c d)
(agenda-execute! #f)

