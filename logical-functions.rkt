#lang racket

;=====================================================================================================

(require "ternary-logic.rkt")

(provide
   Not-function
   And-function
  Nand-function
    Or-function
   Nor-function
   Xor-function
 Imply-function
    If-function)

;=====================================================================================================

(define (  Not-function   trit ) (trit-case trit (T) (F) (?)))
(define (  And-function . trits) (Not-function (apply Nand-function trits)))
(define (   Or-function . trits) (apply Nand-function (map Not-function trits)))
(define (  Nor-function . trits) (Not-function (apply Nand-function (map Not-function trits))))
(define (Imply-function premise consequence) (Nand-function premise (Not-function consequence)))

(define (If-function test then else)
 (Nand-function
  (Nand-function then else)
  (Nand-function test then)
  (Nand-function (Not-function test) else)))

(define (Nand-function . trits)
 (define (Nand-function trits out)
  (cond
   ((null? trits) out)
   (else
    (trit-case (car trits)
     (T)
     ((Nand-function (cdr trits) out))
     ((Nand-function (cdr trits) ?))))))
 (Nand-function trits F))

(define (Xor-function . trits)
 (define (Xor-function trits out)
  (cond
   ((null? trits) out)
   (else
    (trit-case (car trits)
     ((Xor-function (cdr trits) out))
     ((Xor-function (cdr trits) (Not-function out)))
     (?)))))
 (Xor-function trits F))

;=====================================================================================================
; The end
