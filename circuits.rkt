#lang racket

(require "circuits-module.rkt")

(provide
 F  T  ?
 F? T? ??
 trit? trits in-trits trit-case
 bit?  bits  in-bits  bit-case
 trit-combinations bit-combinations
 trit-case bit-case
 wire-make
 wire?
 wire-name
 wire-signal
 wire-init-signal
 define-wires
 report-wire-width
 wire-print
 wire-println
 wire-nr-of-actions
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
 report-time-width
 make-circuit-constr make-gate*-constr
 circuit-constr? circuit-constr-name
 reset-hidden-cntr!
 Not         Not-function
 And And3    And-function
 Nand Nand3  Nand-function
 Or Or3      Or-function
 Nor Nor3    Nor-function
 Xor         Xor-function
; And* Nand* Or* Nor* Xor*   
 Imply       Imply-function
 If          If-function
 Delay)
  