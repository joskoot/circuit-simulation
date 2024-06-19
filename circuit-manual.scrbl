#lang scribble/manual

@;====================================================================================================

@(require
   racket
   racket/function
   scribble/core
   "circuits.rkt"
   "scribble-utensils.rkt"
   racket/block
   (for-label
     "circuits.rkt"
     (except-in racket set)
     racket/block
     racket/function
     racket/block)
   (for-template
     "circuits.rkt"
     (except-in racket set)
     racket/function
     racket/block)
   (for-syntax
     (except-in racket set)
     racket/function
     racket/block))

@(define-for-syntax local #f)

@(define-syntax-rule (cmt x ...) (black (smaller x ...)))

@(define-syntax (nbhll stx)
   (syntax-case stx ()
     ((_ x y ...)
      (if local
        #'(nb (hyperlink x y ...))
        #'(nb (hyperlink (string-append "../../" x) y ...))))))

@(define-syntax (Defmodule stx)
   (if local
     #'(defmodule "circuits.rkt" #:packages ())
     #'(defmodule circuit-simulation/circuits #:packages ())))

@title[#:version ""]{Digital Circuits}

@author{Jacob J. A. Koot}

@(Defmodule)

@(define ternary-table

   @Tabular[
 (("constant" "printed as" "predicate" "description" (list "is a " @nbr[trit?])
              (list "also a " @nbr[bit?]))
  (@nbr[F] @tt{F} @nbr[F?] "false, off, low" "yes" "yes")
  (@nbr[T] @tt{T} @nbr[T?] "true, on, high"  "yes" "yes")
  (@nbr[?] @tt{?} @nbr[??] "indeterminate"   "yes" "no"))
 #:sep (hspace 2)
 #:row-properties '((top-border bottom-border) () () bottom-border)
 #:column-properties '(center center center center center center)])

@section{Preface}

Module @nbhll["circuits.rkt"]{circuits.rkt}
provides tools for digital simulation of digital circuits.
I have used some of the ideas in section 3.3.4 of
“@hyperlink["https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book.html"]{
 Structure and Inter­pretation of Computer Programs}”
by Harold Abelson and Gerald Jay Sussman with Julie Sussman,
in particular the use of an @seclink["agenda"]{agenda}.
My tools differ in many aspects, though, the agenda too.

This document intentionally contains some repetitions of the same or similar phrases in order to
avoid hyperlinks where text is relevant on the spot.

@section[#:tag "introduction"]{Introduction}

Digital circuits consist of @seclink["gate"]{gates} and @seclink["wires"]{wires}.
Circuits are made by circuit constructors,
which them­selves can be made with syntax @racket[make-circuit-constr].
@seclink["gate"]{Gates} are elementary circuits.
Their constructors are provided by module @nbhll["circuits.rkt"]{circuits.rkt}.
For each wire and each occurrence of a gate a distinct instance is made.
Circuits can be nested as subcircuits in an enveloping circuit,
but at the finest grain level, all circuits consist of distinct instances of @seclink["wires"]{wires}
and distinct instances of @nbr[Not], @nbr[Nand], @nbr[Nand3] and @nbr[Delay] gates only.
More gates are available.
These are: @nbr[And], @nbr[And3], @nbr[Or], @nbr[Or3], @nbr[Nor], @nbr[Nor3],
@nbr[Xor], @nbr[Eqv], @nbr[If] and @nbr[Imply].
They are built with @nbr[Not], @nbr[Nand] and @nbr[Nand3] gates.
The names of gates, @nb{or rather} their constructors,
start with a capital letter in order not to shadow procedure @nbr[not]
and syntaxes like @nbr[and], @nbr[nand], @nbr[or] and @nbr[delay]
as provided by @(Rckt).

@seclink["Ternary logic"]{Ternary logic} is used,
@nb{@nbr[F] and @nbr[T]} for a determinate signal
and @nbr[?] for an indeterminate signal, id est, a signal not yet known.
@nb{@nbr[F] represents false cq low}.
@nb{@nbr[T] represents true cq high.}
When @nb{a signal} changes from @nbr[F] to @nbr[T], it is said to raise.
When it changes from @nbr[T] to @nbr[F], it is said to drop.
If all inputs of a @seclink["gate"]{gate} are determinate, its output is determinate too.
Its output is indeterminate if one or more of the inputs are indeterminate
and various determinate outputs can be expected when replacing the indeterminate inputs
by determinate ones.
For example, an @nbr[And] gate gives indeterminate output if one input is @nbr[T]
and the other one is indeterminate, for in this case the output depends on which
determ@roman{@(-?)}inate signal will be given for the indeterminate input.
@nb{If one} input is @nbr[F], the other input is irrelevant and the output is @nbr[F].
Section @nb{@seclink["truth tables"]{Truth tables}} shows
the ternary truth tables of the predefined gates.

A @seclink["gate"]{gate} output can be connected via a @seclink["wires"]{wire}
to the inputs of one or more other gates or even to an input of the same gate itself.
@nb{A wire} with bifurcations to several gate inputs is regarded as one single wire.
A circuit usually has one or more external input and output contacts.
They connect to external input and output wires.
A wire cannot be connected to more than one gate output and
the latter cannot be connected to an external input contact.
Every wire can be an external output wire.
@nb{A wire} that connects a gate output with one or more gate inputs and is not an
external output wire, is an internal wire.
@nb{An external} wire of an embedded circuit can be but not necessarily is
an external wire of the enveloping circuit.
The signals on two or more wires can mutually depend on each other,
@nb{as for example} in a latch or a flip-flop.

A @seclink["wires"]{wire} preserves its signal as long as not mutated.
After mutation, the new signal is preserved up to the next mutation, and so on.
When the signals on one or more inputs of a gate change,
the signal on the output may change.
However, this change always is delayed by at least one small unit of time.
Therefore we use an @seclink["agenda"]{agenda}
that records which wires will change signal and at which times.
When a wire signal changes, all gates that have this wire as input are triggered
such as to compute new outputs and to schedule mutations of the output signals.
When the signals of two or more inputs of a gate change at the same time,
the gate is triggered once only with the new signals on all its inputs.
@nb{If none} of the inputs change, the gate is not triggered.
When a gate is triggered but the new input signals do not change the output,
no mutation of the output wire is scheduled.

@section{How to proceed}
@nb{Simulation} is done by procedure @nbr[agenda-execute!],
but some preparation is required in order to make and install the circuit.
A simulation can be done in various ways, @nb{for example} as follows:

@elemtag{circuit-call}
@itemlist[#:style 'ordered

 @item{@tt{(@nbr[define]
   my-circuit-constr (@nbr[make-circuit-constr] @italic{description} ...))}@(lb)
  Defines a circuit constructor.
  The @italic{@tt{description}} includes a name, external input and output contacts and
  a straightforward list of the elements of
  a @hyperlink["https://en.wikipedia.org/wiki/Electronic_symbol"]{diagram} of the circuit.}

 @item{
  @tt{(@nbr[define]@(hspace 2)@italic{input-wire} (@nbr[wire-make] @italic{name} [@italic{signal}]))
   ...}@(lb)
  @tt{(@nbr[define] @italic{output-wire} (@nbr[wire-make] @italic{name} [@italic{signal}])) ...}@(lb)
  Defines external input and output wires and initializes their signals.@(lb)
  Usually, but not necessarily, @tt{@italic{output-wire}s} are initialized with indeterminate signal
  @nbr[?].}

 @item{@tt{(my-circuit-constr @italic{input-wire} ... @italic{output-wire} ...)}@(lb)
  Installs an instance of the circuit and connects it to its external input and output wires.@(lb)
  The constructor returns @(Void), but does make a circuit behind the screen
  and schedules events in the @seclink["agenda"]{agenda}.
  @nb{No simulation yet.}}

 @item{@tt{(@nbr[agenda-schedule!] @italic{input-wire} @italic{signal} [@italic{delay}]) ...}@(lb)
  Optionally use procedure @nbr[agenda-schedule!] or syntax @nbr[agenda-sequence!]
  to put signals on input wires,
  either without delay or scheduled for a future time.
  More than one such event can be scheduled for the same time and more than one event can be scheduled
  for several times for the same wire.
  Scheduling an event for the same time and wire as already scheduled, cancels the earlier one.}

 @item{@nbr[(agenda-execute!)]@(lb)
  Call procedure @nbr[agenda-execute!] to start or to resume the simulation.
  @nb{The first call} corresponds to power up of the circuit.
  Special care must be taken for a circuit
  with mutual dependency between the signals on its wires.
  Such dependency may cause instability,
  id est, oscillating signals ad infinitum on one or more wires, thus causing an infinite loop.
  Parameter @nbr[agenda-time-limit] protects against such loops:@(lb)
  @(hspace 3)@tt{(@nbr[parameterize] ((@nbr[agenda-time-limit] @italic{time-limit}))
   (@nbr[agenda-execute!]))}@(lb)
  @nb{If you want} a report of the simulation, call @nbr[agenda-execute!] as follows:
  @(hspace 3)@nbr[(agenda-execute! #t)] or as:@(lb)
  @(hspace 3)@nbr[(parameterize ((agenda-report #t)) (agenda-execute!))]@(lb)
  A report shows all mutations on the wires and at which times, internal wires included.}

 @item{In addition to procedure @nbr[wire-signal] and any format or output procedure of @(Rckt),
  procedure @nbr[wire-print] or @nbr[wire-println] can be used
  to see the final signals on the output wires.
  Simply typing the identifiers of wires will do too.}

 @item{Halt here or repeat from step 4.}]

For examples see the next section and section @seclink["More examples"]{More examples}.

@section[#:tag "D-flip-flop-section"]{Example: a D-flip-flop}

As introductory example a D-flip-flop.@(lb)
It has two inputs, say @tt{in} and @tt{clock}.@(lb)
It has two outputs, say @tt{state} and @nb{@tt{state-inverse}}.@(lb)
Once stable, the outputs remain inverses of each other@(lb)
and the following transition table applies:

@Tabular[
 (((tt "clock")
   (tt "in")
   (list "old" (hspace 1) (tt "state"))
   "│"
   (list "new" (hspace 1) (tt "state"))
   (list "new" (hspace 1) (tt "state-inverse"))
   "action")
  (@nbr[T] @nbr[F] "any"   "│" @nbr[F] @nbr[T] "reset")
  (@nbr[T] @nbr[T] "any"   "│" @nbr[T] @nbr[F] "set")
  (@nbr[F] "any"   @nbr[F] "│" @nbr[F] @nbr[T] "state preserved")
  (@nbr[F] "any"   @nbr[T] "│" @nbr[T] @nbr[F] "state preserved"))
 #:row-properties '((top-border bottom-border) () () () bottom-border)
 #:column-properties '(center center center center center center left)
 #:sep (hspace 2)]

Hence, in order to set or reset the @nb{D-flip-flop},
set @tt{in} to @nbr[T] cq @nbr[F] and apply a @nbr[T] pulse to the @tt{clock}.
Leave the @tt{clock} low at @nbr[F] in order to preserve the state
without being disturbed by changes on wire @tt{in}.
There are several ways to construct a D-flip-flop.
The following diagram shows one consisting of four @nbr[Nand] gates.

@elemtag["D-flip-flop-diagram"]@inset{@image["D-flip-flop.gif" #:scale 1]}

The D-flip-flop must be stabilized with a @nbr[T] pulse on the @tt{clock} and @tt{in} hold at
@nbr[F] or @nbr[T].
When @tt{clock}=@nbr[F] the signals on wires @tt{set} and @tt{reset} raise to @nbr[T]
and the two @nbr[Nand] gates at the right of the diagram act as inverters
for the signals on wires @tt{state} and @tt{state-inverse}.
This means that these signals will oscillate during power up
with @tt{clock}=@nbr[F] and the same determinate signal @nbr[F] or @nbr[T]
on wires @tt{state} and @tt{state-inverse}.
Raising the @tt{clock} to @nbr[T] stabilizes the @tt{state} and @nb{@tt{state-inverse}}
such as to become inverses of each other.
@nb{Once stabilized} they always end up as inverses of each other.
This is shown in the sequel.

With syntax @nbr[make-circuit-constr] a constructor for a simulator
can be made by straightforwardly listing the elements of the diagram:

@Interaction*[
 (define D-flip-flop-constr
   (make-circuit-constr
     'D-flip-flop          (code:comment "name")
     (in clock)            (code:comment "external input contacts")
     (state state-inverse) (code:comment "external output contacts")
     (code:comment "gates: four distinct instances")
     (code:comment "output       (Gate input ...)")
     (reset         (Nand clock in))
     (set           (Nand clock reset))
     (state         (Nand reset state-inverse))
     (state-inverse (Nand set   state))))]

The order in which the gates (or subcircuits) are listed is irrelevant.
Syntax @nbr[make-circuit-constr] yields @nb{a circuit} constructor.
Now define the external input and output wires.
@nb{The internal} wires @tt{set} and @tt{reset} are taken care of by the circuit constructor.
In this example @tt{state-wire} and @tt{state-inverse-wire} are initialized with signal @nbr[F]
in stead of the default indeterminate signal @nbr[?],
because this better demonstrates the vibration during power up with @nb{@tt{clock}=@nbr[F].}
At power up time the signal on the four wires is @nbr[F].

@Interaction*[
 (define in-wire            (wire-make 'in            F))
 (define clock-wire         (wire-make 'clock         F))
 (define state-wire         (wire-make 'state         F))
 (define state-inverse-wire (wire-make 'state-inverse F))]

Now install the D-flip-flop. Procedure @tt{D-flip-flop-constr} returns @(Void).
It wants four wires for its arguments:
@tt{in} and @tt{clock} for its inputs and @tt{state} and @tt{state-inverse} for its outputs.
It makes an instance of a D-flip-flop
and connects the input and output wires to the circuit.
@nb{It also} schedules events in the agenda for power up. @nb{No simulation yet.}

@Interaction*[(D-flip-flop-constr in-wire clock-wire state-wire state-inverse-wire)]

Put instructions in the agenda such as to apply a @nbr[T] pulse
on the @tt{clock} from time @nbr[10] to time @nbr[20].
No simulation yet.
For times @nbrl[natural?]{natural numbers} are used without specifying a unit.
This unit always is implied and always the same.

@Interaction*[
 (code:comment "------------------------------------- clock is F at time  0")
 (code:line (agenda-schedule! clock-wire T 10) (code:comment "raise clock at time 10"))
 (code:line (agenda-schedule! clock-wire F 20) (code:comment " drop clock at time 20"))]

Now the agenda contains all information needed for the simulation.
Procedure @nbr[agenda-execute!] does the simulation.
Parameter @nbr[agenda-report] is set to @nbr[#t] indicating that procedure @nbr[agenda-execute!]
must print a report.
Parameters @nbr[report-time-width] and @nbr[report-wire-width]
are used for alignment in the report.

@Interaction*[
 (define (D-flip-flop-simulator)
   (display " \nInitial state: ")
   (wire-println state-wire state-inverse-wire)
   (display "Simulation:\n")
   (parameterize
     ((agenda-report #t)
      (report-time-width 2)
      (report-wire-width 13))
     (agenda-execute!))
   (display "Final state: ")
   (wire-println state-wire state-inverse-wire))]
@Interaction*[
 (D-flip-flop-simulator)]

Up to time 10 the clock is @nbr[F]
and the signals on wires @tt{state} and @tt{state-inverse} oscillate.
At time 10 the clock is raised to @nbr[T].
This stabilizes @tt{state} and @tt{state-inverse} within 3 time steps
such as to become inverses of each other.
After time 12 nothing happens until at time 20 the clock is dropped to @nbr[F],
with the only effect that the signal on internal wire @tt{set} raises to @nbr[T].
At this time wire @tt{reset} has signal @nbr[T] too,
but because the @tt{state} and @tt{state-inverse} already are inverses of each other,
they remain stable.

At this moment the D-flip-flop is in reset state. Now setting it
by applying a @nbr[T] pulse on the @tt{clock-wire} for 2 units of time with @tt{in-wire}=@nbr[T],
the @tt{state} and @nb{@tt{state-inverse}} switch almost simultaneously,
but not quite:

@Interaction*[
 (agenda-schedule! in-wire    T)
 (agenda-schedule! clock-wire T)
 (agenda-schedule! clock-wire F 2)
 (D-flip-flop-simulator)]

The @tt{state} switched at time 24, whereas @tt{state-inverse} changed at time 25.@(lb)
During one unit of time @tt{state} and @tt{state-inverse} had the same signal @nbr[T].@(lb)
The dropping output always comes one unit of time later than the raising one.@(lb)
Notice that the time does not avance while procedure @nbr[agenda-execute!] is not running.@(lb)
The time is not reset while returning from procedure @nbr[agenda-execute!].

To set or reset the D-flip-flop a @nbr[T]-puls of two time units is sufficient,
but a pulse of only one time unit may be too short,
causing the D-flop-flop to oscillate:

@Interaction*[
 (agenda-schedule! in-wire F)
 (agenda-schedule! clock-wire T)
 (code:line (agenda-schedule! clock-wire F 1) (code:comment "Too short clock pulse."))
 (parameterize
   ((agenda-time-limit (+ (agenda-time) 5))
    (agenda-report #t))
   (D-flip-flop-simulator))]

Even with a longer clock pulse the signal on @tt{state-inverse-wire} may flip to @nbr[T]
@nb{for a moment} when setting the flip-flop while it already is set:

@Interaction*[
 (code:comment #, @cmt{set:})
 (agenda-schedule! in-wire T)
 (agenda-schedule! clock-wire T)
 (agenda-schedule! clock-wire F 5)
 (agenda-execute!)
 (code:comment #, @cmt{set while already set: @tt{state-inverse} flips to @nbr[T] for a moment:})
 (agenda-schedule! clock-wire T)
 (agenda-schedule! clock-wire F 5)
 (parameterize ((report-wire-width 13)) (agenda-execute! #t))]

This can be avoided by replacing
@inset{
 @nbr[(set (Nand clock reset))] by @(lb)
 @nbr[(set (Nand ((Delay 1) clock) reset))]}
in order to trigger the @nbr[Nand]-gate with output wire @tt{set}
after wire @tt{reset} has obtained its new value.
In fact this gate is not triggered when setting the flip-flop while already set:

@Interaction*[
 (define D-flip-flop-constr-with-delay
   (make-circuit-constr
     'D-flip-flop-with-delay (code:comment "name")
     (in clock)             (code:comment "external input contacts")
     (state state-inverse)  (code:comment "external output contacts")
     (code:comment "gates: four distinct instances")
     (code:comment "output       (Gate input ...)")
     (reset         (Nand clock in))
     (delayed-clock ((Delay 1) clock))
     (set           (Nand delayed-clock reset))
     (state         (Nand reset state-inverse))
     (state-inverse (Nand set   state))))
 (let ()
   (define in-wire (wire-make 'in T))
   (define clock-wire (wire-make 'clock T))
   (define state-wire (wire-make 'state))
   (define state-inverse-wire (wire-make 'state-inverse))
   (D-flip-flop-constr-with-delay
     in-wire
     clock-wire
     state-wire
     state-inverse-wire)
   (displayln "Set:")
   (agenda-execute!)
   (agenda-schedule! clock-wire F)
   (agenda-execute!)
   (wire-println state-wire state-inverse-wire)
   (displayln "Set while already set:")
   (displayln "Nothing happens on wires state and state-inverse:")
   (agenda-schedule! clock-wire T)
   (agenda-schedule! clock-wire F 2)
   (parameterize ((report-wire-width 13)) (agenda-execute! #t))
   (wire-println state-wire state-inverse-wire))]

Let's test the D-flip-flop for all binary combinations of @tt{in}, @tt{clock} and old @tt{state}.

@Interaction*[
 (define (test-D-flip-flop flip-flop-constr #:tabulate? (tabulate? #f))
   (printf " ~nTesting ~s.~n" (circuit-constr-name flip-flop-constr))
   (code:comment " ")
   (code:comment "Procedure test/tabulate does a test. If tabulate? is true,")
   (code:comment "it also prints a line of results for the table to be shown.")
   (code:comment " ")
   (define (test/tabulate in-signal clock-signal old-state)
     (code:comment " ")
     (code:comment "First put the flip-flop in the desired old-state.")
     (code:comment " ")
     (agenda-schedule! in-wire old-state)
     (agenda-schedule! clock-wire T)
     (agenda-execute!)
     (agenda-schedule! clock-wire F)
     (agenda-execute!)
     (code:comment " ")
     (code:comment "Check that the flip-flop has assumed the desired state.")
     (code:comment " ")
     (unless
       (and
         (eq? (wire-signal state-wire) old-state)
         (eq? (wire-signal state-inverse-wire) (Not-function old-state)))
       (error 'flip-flop "test fails"))
     (code:comment " ")
     (code:comment "Send in-signal and clock-signal to the flip-flop.")
     (code:comment " ")
     (agenda-schedule! in-wire in-signal)
     (agenda-schedule! clock-wire clock-signal)
     (agenda-execute!)
     (code:comment " ")
     (code:comment "Check the results.")
     (code:comment " ")
     (define new-state-signal         (wire-signal state-wire))
     (define new-state-inverse-signal (wire-signal state-inverse-wire))
     (or
       (and
         (F? clock-signal)
         (eq? new-state-signal old-state)
         (eq? new-state-inverse-signal (Not-function old-state)))
       (and
         (T? clock-signal)
         (eq? new-state-signal in-signal)
         (eq? new-state-inverse-signal (Not-function in-signal)))
       (error 'flip-flop "test fails"))
     (code:comment " ")
     (code:comment "If desired print results.")
     (code:comment " ")
     (when tabulate?
       (printf "old-state=~s  " old-state)
       (wire-println #:sep "  "
         in-wire clock-wire state-wire state-inverse-wire)))
   (code:comment " ")
   (code:comment "───────────────────────────────────────────────────────────────")
   (code:comment "Install the flip-flop.")
   (code:comment " ")
   (D-flip-flop-constr in-wire clock-wire state-wire state-inverse-wire)
   (code:comment " ")
   (code:comment "───────────────────────────────────────────────────────────────")
   (code:comment "First print a header for the table to be printed (if desired).")
   (code:comment " ")
   (when tabulate?
     (displayln "────────────────────────────────────────────────────")
     (displayln "old-state    inputs         new state")
     (displayln "───────────  ─────────────  ────────────────────────"))
   (code:comment " ")
   (code:comment "───────────────────────────────────────────────────────────────")
   (code:comment "Now do the tests. Each test adds a line to the table (if desired).")
   (code:comment "Call test/tabulate for all feasible binary inputs.")
   (code:comment " ")
   (for*
     ((old-state    in-bits)
      (clock-signal in-bits)
      (in-signal    in-bits))
     (test/tabulate in-signal clock-signal old-state))
   (code:comment " ")
   (code:comment "───────────────────────────────────────────────────────────────")
   (code:comment "Close the table.")
   (code:comment " ")
   (when tabulate?
     (displayln "────────────────────────────────────────────────────"))
   (code:comment " ")
   (code:comment "───────────────────────────────────────────────────────────────")
   (code:comment "Arriving here means that all tests passed.")
   (code:comment " ")
   (printf "Hurray, all tests passed.~n"))]

Now we have procedure @tt{test-D-flip-flop} and can use it:

@Interaction*[(test-D-flip-flop D-flip-flop-constr #:tabulate? #t)]

Let's test the D-flip-flop with delayed clock too:

@Interaction*[(test-D-flip-flop D-flip-flop-constr-with-delay)]

The two @nbr[Nand] gates at the right of the @elemref["D-flip-flop-diagram"]{diagram}
form an odd kind of SR-latch.@(lb)
Compared to a regular SR-latch it interprets its S and R input as their inverses.@(lb)
@tt{S-inverse} corresponds to wire @tt{reset} in the @elemref["D-flip-flop-diagram"]{diagram}.@(lb)
@tt{R-inverse} corresponds to wire @tt{set} in the @elemref["D-flip-flop-diagram"]{diagram}.

@Tabular[
 ((@tt{S-inverse} @tt{R-inverse} "|" @tt{state} @tt{state-inverse} "action")
  ((nbr F) (nbr F) "|"  (nbr T) (nbr T) (red "don't do this"))
  ((nbr F) (nbr T) "|"  (nbr T) (nbr F) "set")
  ((nbr T) (nbr F) "|"  (nbr F) (nbr T) "reset")
  ((nbr T) (nbr T) "|"  @tt{state} @tt{state-inverse} "preserve state"))
 #:row-properties '((top-border bottom-border) () () () bottom-border)
 #:column-properties '(center center center center center left)
 #:sep (hspace 2)]
The last line of this transition table applies after the latch has been stabilized@(lb)
by setting or resetting it.
We can use the latch as a subcircuit in a D-flip-flop:

@Interaction*[
 (define make-odd-SR-latch
   (make-circuit-constr 'SR-latch
     (S-inverse R-inverse) (code:comment "inputs")
     (state state-inverse) (code:comment "outputs")
     (code:comment "gates:")
     (state         (Nand S-inverse state-inverse))
     (state-inverse (Nand R-inverse state))))]
@Interaction*[
 (define make-D-flip-flop-with-odd-SR-latch
   (make-circuit-constr 'D-flip-flop-with-odd-SR-latch
     (in clock)            (code:comment "inputs")
     (state state-inverse) (code:comment "outputs")
     (code:comment "gates and subcircuit:")
     (S-inverse (Nand clock in))
     (R-inverse (Nand clock S-inverse))
     ((state state-inverse) (make-odd-SR-latch S-inverse R-inverse))))]
@Interaction*[
 (test-D-flip-flop make-D-flip-flop-with-odd-SR-latch)]
@(reset-Interaction*)

More examples in section @seclink["More examples"]{More examples}.

@section[#:tag "Ternary logic"]{Ternary logic}

@ternary-table

@deftogether[
 (@defthing[#:kind "false/off/low, constant" F (and/c trit? bit?)]
   @defthing[#:kind "true/on/high, constant" T (and/c trit? bit?)]
   @defthing[#:kind "indeterminate, constant" ? (and/c trit? (not/c bit?))]
   @defthing[#:kind "constant" trits (list/c bit? bit? trit?) #:value (list F T ?)]
   @defthing[#:kind "constant" in-trits sequence? #:value (in-list trits)]
   @defproc[#:kind "predicate" (trit? (obj any/c)) boolean?]
   @defproc[#:kind "predicate" (F? (obj any/c)) boolean?]
   @defproc[#:kind "predicate" (T? (obj any/c)) boolean?]
   @defproc[#:kind "predicate" (?? (obj any/c)) boolean?])]{

 @nbr[(trit? obj)] yields @nbr[#t] if @nbr[obj] is a trit, id est, @nbr[F], @nbr[T] or @nbr[?].@(lb)
 Else @nbr[(trit? obj)] yields @nbr[#f].@(lb)
 @nbr[(F? obj)] is the same as @nbr[(eq? obj F)].@(lb)
 @nbr[(T? obj)] is the same as @nbr[(eq? obj T)].@(lb)
 @nbr[(?? obj)] is the same as @nbr[(eq? obj ?)].}

@defform[(trit-case trit-expr (F-body ...) (T-body ...) (?-body ...))
         #:contracts ((trit-expr trit?))]{
 Evaluates the @nbr[F-body], @nbr[T-body] or @nbr[?-body]
 depending on the value of the @nbr[trit-expr].
 @nb{The selected} @italic{@tt{body}} is evaluated in tail position. The other ons are ignored.
 @nb{A @tt{@italic{body}}} can include definitions, but the last element must be an expression.
 @nb{A @tt{@italic{body}}} may be empty, @nb{in which} case its value is @(Void).}

@defproc[(trit-combinations (n natural?)
           (sort? any/c #f)
           (#:vector vector? any/c #f))
         (or/c (listof (listof trit?)) (vectorof (listof trit?)))]{
 Procedure @nbr[trit-combinations] returns a list or vector of all combinations of @nbr[n] trits.@(lb)
 The result is a list or vector of 3@↑{n} lists of @nbr[n] trits. If @nbr[sort?] has a true value,
 combinations with determinate trits (bits) only precede all other ones.

 @Interaction[
 (trit-combinations 2 #f)
 (trit-combinations 2 #t)]

 The combinations can be used to check a circuit for all feasible inputs@(lb)
 or for making a truth table:

 @Interaction[
 (for ((combination (in-list (trit-combinations 2 #t))))
   (define a (car combination))
   (define b (cadr combination))
   (define wa  (wire-make 'a a))
   (define wb  (wire-make 'b b))
   (define out (wire-make 'out))
   (And wa wb out)
   (agenda-execute!)
   (printf "(And ~s ~s) = ~s~n" a b (wire-signal out)))]}

@section[#:tag "binary"]{Binary logic}

The binary digits are @nbr[F] and @nbr[T].@(lb)
They are @seclink["Ternary logic"]{trits} too.@(lb)
Always @nbr[(implies (bit? x) (trit? x))] → @nbr[#t].

@deftogether[
 (@defthing[#:kind "constant" bits (list/c bit? bit?) #:value (list F T)]
   @defthing[#:kind "constant" in-bits sequence? #:value (in-list bits)]
   @defproc[#:kind "predicate" (bit? (obj any/c)) boolean?])]{
 If the @nbr[obj] is a bit, id est, @nbr[F] or @nbr[T], @nbr[(bit? obj)] yields @nbr[#t],
 else it yields @nbr[#f].}

@defform[(bit-case bit-expr (F-body ...) (T-body ...))
         #:contracts ((bit-expr bit?))]{
 Evaluates the @nbr[F-body] or @nbr[T-body]
 depending on the value of the @nbr[bit-expr].
 The selected @italic{@tt{body}} is evaluated in tail position. The other one is ignored.
 @nb{A @tt{@italic{body}}} can include definitions, but the last element must be an expression.
 @nb{A @tt{@italic{body}}} may be empty, @nb{in which} case its value is @(Void).}

@defproc[(bit-combinations (n natural?) (#:vector vector? any/c #f))
         (or/c (listof (listof bit?)) (vectorof (listof bit?)))]{
 Procedure @nbr[bit-combinations] returns a list or vector of all combinations of @nbr[n] bits.@(lb)
 The result is a list or vector of 2@↑{n} lists of @nbr[n] bits.

 @Interaction[
 (bit-combinations 3 #:vector #t)]}

@section[#:tag "agenda"]{Agenda}

An agenda schedules events, which are instructions to put a new signal on a given wire
at a given time in future.
More than one event can be scheduled for the same time.
For times @nbrl[natural?]{natural numbers} are used without specifying a unit of time.
This unit is implied and always the same, @nb{may be not} more than a small fraction of a nanosecond.
Every agenda has a clock called ‘@nbr[agenda-time]’.
@nb{An agenda} never schedules events preceding the current @nbr[agenda-time].
Events are @nbrl[agenda-execute!]{executed} in order of progressing time.
During a simulation the circuit may schedule more events.
These are handled too.
After execution of the events for time t
the @nbr[agenda-time] is increased to t+1.
An agenda can contain more than one event for the same time and
for several input wires of the same gate.
In that case the gate is triggered once after all these wires have received their new signals.
As long as none of the input wires of a gate change signal, @nb{the gate} is not triggered.
When a gate is triggered with input signals that do not change the output,
no event is scheduled for this output.

@defparam[current-agenda agenda agenda? #:value (agenda-make 'the-agenda)]{

 All procedures that need an agenda use the @nbr[current-agenda].
 You probably never need to mutate this parameter.
 It is necessary, though, to use separate agendas
 when running simulations simultaneously in threads, places or futures.
 This is possible provided the circuits are distinct and do not share output wires.}

@defproc[(agenda-make (name symbol? 'no-name)) agenda?]{
                                               
 Returns an empty agenda with @nbr[agenda-time] @nbr[0].
 The @nbr[name] is for its printed form:

 @Interaction[
 (agenda-make)
 (object-name (agenda-make 'my-agenda))]}

@defproc[#:kind "predicate" (agenda? (obj any/c)) boolean?]{

 Predicate for agendas made by procedure @nbr[agenda-make].}

@defproc[(agenda-reset!) void?]{

 Removes all events from the @nbr[current-agenda] and resets its @nbr[agenda-time] to @nbr[0].
 Usually @nbr[agenda-reset!] is called when the @nbrl[current-agenda]{agenda} already is empty,
 just in order to reset its time.}

@defproc[(agenda-empty?) boolean?]{

 Returns @nbr[#t] if the @nbr[current-agenda] is empty, else @nbr[#f].
 This procedure is useful only to check whether or not the installation of a circuit did
 schedule events.
 Installing a circuit usually schedules events, but not always.
 If none of the gates will change their outputs during power up, no events are scheduled.
 After return from procedure @nbr[agenda-execute!] the related agenda always is empty.

 @Interaction[
 (Not (wire-make 'in F) (wire-make 'out F))
 (agenda-empty?)
 (agenda-execute! #t)
 (agenda-empty?)]

 @Interaction[
 (Not (wire-make 'in F) (wire-make 'out T))
 (agenda-empty?)
 (code:line (agenda-execute! #t) (code:comment "Nothing happens"))]}

@defproc[(agenda-time) natural?]{

 Returns the current time of the @nbr[current-agenda].}

@defproc[(agenda-execute! (report any/c (agenda-report))) void?]{

 Executes all events scheduled in the @nbr[current-agenda] in order of increasing time.
 While @racket[agenda-execute!] is running,
 the simulated circuit may schedule more events.
 These will be executed too.
 Procedure @nbr[agenda-execute!] continues until the @nbr[current-agenda] is empty
 or the @racket[agenda-time-limit] is exceeded.
 When procedure @nbr[agenda-execute!] finishes, the @nbr[agenda-time] is left
 one unit of time after that of the last event.
 After all wires have obtained their final values, the agenda may still contain events
 scheduled explicitly by means of procedure @nbr[agenda-schedule!] or syntax @nbr[agenda-sequence!]
 but that will not change any signal.
 @nb{In that} case time keeps running while walking the agenda.
 Events that in future will not change the signal on
 @nb{a wire} cannot be ignored, because without actually continuing the simulation,
 in general it cannot be known which signal the wire will have when the time of the event will come.
 @nb{The time} is not running while procedure @nbr[agenda-execute!] is not running.
 The agenda never contains more than one event for the same wire and the same time.
 If @nbr[report] has a true value, a report is printed showing
 all signal switches including time, name of the wire, old signal and new signal.
 @nb{For an} example see the @nb{@seclink["D-flip-flop-section"]{D-flip-flop example}}.
 Events for the same time segment are reported in sorted order of the names of the wires.
 If parameter @nbr[report-hidden] is @nbr[#f] wires whose names end with a dot ‘•’
 followed by one or more decimal digits are not included in the report.
 Such names are used for @nbrl[report-hidden]{hidden wires.}
 Procedure @nbr[agenda-execute!] can handle more than one circuit simultaneously,
 provided they do not share external output wires:

 @Interaction[
 (define a (wire-make 'a T))
 (define b (wire-make 'b T))
 (define c (wire-make 'c F))
 (And a b (wire-make 'and-ab))
 (And a c (wire-make 'and-ac))
 (Xor a b (wire-make 'xor-ab))
 (Xor a c (wire-make 'xor-ac))
 (agenda-execute! #t)]}

@defparam[agenda-time-limit time-limit (or/c natural? #f) #:value 1000]{

 Parameter forcing a limit on the progression of the @nbr[agenda-time].
 When procedure @nbr[agenda-execute!] goes beyond the @nbr[time-limit], an exception is raised.
 No time-limit is imposed if @nbr[time-limit] is @nbr[#f]
 in which case the simulator of an oscillating circuit keeps running forever (in bound space).
 The limit does not apply to procedure @nbr[agenda-schedule!] nor to syntax @nbr[agenda-sequence!],
 such as to allow the @nbr[agenda-time-limit] @nb{to be} adjusted after scheduling events.

 @Interaction[
 (wire-init-signal F)
 ((make-circuit-constr 'oscillator () () (a (Not a))))
 (agenda-time-limit 9) 
 (agenda-execute! #t)]

 @note{Considere the number of different internal states of a circuit in combination with
  the agenda modulo @nbr[agenda-time].
  This number is finite.
  Therefore, it is possible to adapt the simulator such as to detect infinite oscillation.
  However, this would involve much overhead in terms of both memory usage and execution time.
  Therefore, this detection has not been implemented.@(lb)
  May be this detection should be added as an option. May be a yet-to-do.}}

@defproc[(agenda-schedule!
           (wire wire?)
           (signal trit?)
           (delay natural? 0)) void?]{

 Schedules in the @nbr[current-agenda] a @nbr[signal] change at time @nbr[(+ (agenda-time) delay)]
 for the @nbr[wire].
 Scheduling an event for the same time and wire as already scheduled,
 cancels the previous one.
 A circuit never schedules events that do not change the signals on the related wires.
 However, by means of procedure @nbr[agenda-schedule!] or syntax @nbr[agenda-sequence!]
 the user can schedule events that do not change signals.
 These events are skipped when their time has come.
 Events that in future will not change the signal on
 @nb{a wire} cannot be ignored, because without actually continuing the simulation,
 in general it cannot be known which signal the wire will have when the time of the event will come.

 @Interaction[
 (define a (wire-make 'a F))
 (agenda-report #t)
 (agenda-schedule! a T 10)
 (agenda-execute!)
 (agenda-time)
 (code:line (agenda-schedule! a T 10) (code:comment "Scheduled for time 11+10=21."))
 (code:line (agenda-execute! #t) (code:comment "Nothing happens, for the signal does not change,"))
 (code:line (agenda-time)        (code:comment "but time kept running."))]}

@defform[(agenda-sequence! (wire (signal delay) ...) ...)
         #:contracts ((wire wire?) (signal trit?) (delay natural?))]{
                                                            
 Same as:

 @inset{@nbr[(agenda-schedule! wire signal delay)] @tt{...}}

 for each @nbr[wire] and per @nbr[wire] for each combination of @nbr[signal] and @nbr[delay].@(lb)
 The events are scheduled in the order they appear in the @nbr[agenda-sequence!] form.@(lb)
 When more than one event appears for the same @nbr[wire] and the same @nbr[delay],@(lb)
 the last one cancels all preceding ones.

 @Interaction[
 (define-wires a b)
 (agenda-sequence!
   (a (F 5) (T 5) (F 6))
   (b (T 7) (F 8) (F 90)))
 (agenda-execute! #t)
 (agenda-time)]

 Event @nbr[(a (F 5))] is canceled by event @nbr[(a (T 5))].@(lb)
 Event @nbr[(b (F 90))] is not executed, because it does not alter the signal.@(lb)
 Nevertheless, time kept running until encountering this event.}

@defparam*[agenda-report on/off any/c boolean?]{
                                                
 Default for argument @tt{@italic{report}} of procedure @nbr[agenda-execute!].}

@defparam[report-time-width minimal-width natural? #:value 1]{
                                                      
 For alignment of the times as printed by procedure @nbr[agenda-execute!] in a report.@(lb)
 For a neat report set the parameter to the maximal number of digits expected in the display
 of a time. A time requiring more than @nbr[minimal-width] decimal digits is not truncated.}

@defparam[report-wire-width minimal-width natural? #:value 1]{
                                                      
 For alignment of the names of wires as printed by procedure @nbr[agenda-execute!] in a report.
 For a neat report set the parameter to the maximal length of wire names.
 A wire name of more than @nbr[minimal-width] characters is not truncated.}

@defparam*[report-hidden on/off any/c boolean? #:value #f]{
                                                           
 If this parameter is @nbr[#f] a report does not show changes of hidden wires.
 A wire is hidden if its name ends with a dot ‘•’ (@green{@tt{#\u2022}})
 followed by one or more decimal digits.
 @nb{A wire} made by syntax @nbr[make-circuit-constr] for the output of
 @nb{a @tt{@italic{subcircuit-arg}}} that itself is 
 @nb{a @tt{@italic{subcircuit}}} has such a name.
 A wire made by procedure @nbr[wire-make] may have such name too.
 Such a wire will be treated as a hidden one too.

 @Interaction[
 (define (install-and-run-circuit show-hidden?)
   (reset-hidden-cntr!)
   (agenda-reset!)
   ((make-circuit-constr 'Circuit (a b) (out)
      (out (Nand (Nand b (Not a)) (Nand a (Not b)))))
    (code:comment "      out   Nand•1  Not•2    Nand•3  Not•4")
    (wire-make 'a F)
    (wire-make 'b T)
    (wire-make 'out))
   (parameterize
     ((agenda-report #t)
      (report-hidden show-hidden?)
      (report-wire-width 6))
     (agenda-execute!)))
 (code:line (install-and-run-circuit #f) (code:comment "Hidden wires not shown."))
 (code:line (install-and-run-circuit #t) (code:comment "Hidden wires are shown."))]}

@defproc[(reset-hidden-cntr!) void?]{
 Resets the counter for naming @nbrl[report-hidden]{hidden} wires.
 Does not affect the identity of hidden wires. Distinct wires can have the same
 @nbrl[wire-name]{name}.}

@section[#:tag "wires"]{Wires}

A wire has a name, a signal and a list of actions.
The signal always is a @nbrl[trit?]{trit}.
Each action is related to a gate that has the wire as an input.
An action triggers the gate when the signal on the wire is changed.
A gate with two or more inputs never is triggered more than once at the same
@nbsl["agenda"]{agenda-time}.
It is triggered once after all inputs have been computed.@(lb)
If the input signals do not change, the gate is not triggered.
A gate does not schedule an event for its output if this output will not change.

@defproc[(wire-make (name symbol?) (signal trit? (wire-init-signal))) wire?]{
                                                                             
 Yields a wire with a @nbr[name], initialized with @nbr[signal]
 and with empty list of actions.@(lb)
 Circuit constructors add actions to wires.

 @Interaction[
 (wire-make 'my-wire T)
 (object-name (wire-make 'another-wire))]
 Distinct wires can have the same name.@(lb)
 Obviously, this may lead to confusion.
 @Interaction[
 (define-values (wire-a wire-b) (values (wire-make 'a) (wire-make 'a)))
 (equal? wire-a wire-b)
 (wire-println wire-a wire-b)
 (agenda-schedule! wire-a T)
 (agenda-execute!)
 (wire-println wire-a wire-b)]}

@defparam[wire-init-signal signal trit? #:value ?]{
                                                   
 Contains the default value for argument @tt{@italic{signal}} of procedure @nbr[wire-make].
 Also used by circuit constructors for the creation of internal wires.
 When @nbr[signal]=@nbr[?], there is a good chance
 that a circuit does not vibrate during power up, whereas it would vibrate
 with a determinate initial signal.
 The reason is that the inverse of @nbr[?] is @nbr[?]. For example:
 @Interaction[
 (define (oscillator init-signal)
   (parameterize
     ((wire-init-signal init-signal)
      (agenda-time-limit 7)
      (agenda-report #t))
     ((make-circuit-constr 'Oscillator () () (a (Not a))))
     (agenda-execute!)
     (agenda-reset!)))
 (code:line (oscillator ?) (code:comment "Nothing happens because the inverse of ? is ?."))
 (code:line (oscillator F) (code:comment "Oscillates, terminated by a time limit error."))]}

@defproc[#:kind "predicate" (wire? (obj any/c)) boolean?]{
                                                          
 Predicate for wires made with procedure @nbr[wire-make].}

@defproc[(wire-name (wire wire?)) symbol?]{
                                           
 Returns the name of the @nbr[wire].@(lb)
 The name can also be retrieved by means of procedure @nbr[object-name].
 @Interaction[
 (define wire (wire-make 'wire))
 (wire-name wire)
 (object-name wire)]}

@defproc[(wire-signal (wire wire?)) trit?]{
                                           
 Returns the current signal on the @nbr[wire].

 @Interaction[
 (wire-signal (wire-make 'who-cares? T))]}

@ignore{@defproc[(wire-set-signal! (wire wire?) (signal trit?)) void?]{
  Assigns the @nbr[signal] to the @nbr[wire].

  @note{Use this procedure cautiously.
   Procedure @nbr[agenda-schedule!] is to be preferred.}}}

@defproc[(wire-print (#:port port (or/c 'current 'string output-port?) 'current) (wire wire?) ...)
         (or/c string? void?)]{

 Displays the names of the @nbr[wire]s and their current signals
 on the @nbr[port] or returns a string.@(lb)
 If @nbr[port] is @nbr['current] the @nbr[current-output-port] is used and @(Void) is returned.@(lb)
 If @nbr[port] is an output-port, that port is used and @(Void) is returned.@(lb)
 If @nbr[port] is @nbr['string] the output is returned as a string.@(lb)
 Every wire is displayed on a separate line, but if @nbr[port] is @nbr['string],@(lb)
 the newline at the end is omitted.

 @Interaction[
 (define-wires (a F) (b T))
 (wire-print a b)]}

@defproc[(wire-println
           (#:port port (or/c 'current 'string output-port?) 'current)
           (#:sep sep any/c " ")
           (wire wire?) ...)
         (or/c string? void?)]{

 Displays the names of the @nbr[wire]s and their current signals
 on the @nbr[port] or returns a string.@(lb)
 If @nbr[port] is @nbr['current] the @nbr[current-output-port] is used and @(Void) is returned.@(lb)
 If @nbr[port] is an output-port, that port is used and @(Void) is returned.@(lb)
 If @nbr[port] is @nbr['string] the output is returned as a string.@(lb)
 The information is displayed in one line, separated by @nbr[sep]
 and with a newline at the end.
 If @nbr[port] is @nbr['string], the newline at the end is omitted.

 @Interaction[
 (define-wires (a F) (b T))
 (wire-println a b #:port 'string #:sep " and ")]}

@defform-remove-empty-lines[
 @defform[(define-wires def ...)
          #:grammar
          ((def id (id) (id signal-expr) (id name-expr) (id name-expr signal-expr)))
          #:contracts ((name-expr symbol?) (signal-expr trit?))]{
                                             
  Same as:

  @inset{@nbr[(define-values (id ...) (values (wire-make name-expr signal-expr) ...))]}

  The default @nbr[name-expr] is @tt{'}@nbr[id] and the default @nbr[signal-expr] is
  @nbr[(wire-init-signal)].
  @Interaction[
 (define-wires a (b) (c ?) (d F) (e 'T T))
 (wire-println a b c d e)]}]
@(reset-Interaction*)

@defproc[(wire-nr-of-actions (wire wire?)) natural?]{
 Returns the number of actions currently attributed to a @nbr[wire],
 id est, the number of gates that have the wire as an input.
 Giving the wire as an input wire to a circuit constructor usually,
 but not necessarily increments this number.

 @Interaction[
 (define-wires a b)
 (And a a b)
 (wire-nr-of-actions a)
 (code:line ((make-circuit-constr 'no-name (a) (a)) a) (code:comment "Does not add an action."))
 (wire-nr-of-actions a)]}

@section{Circuit constructors}

Syntax @nbr[make-circuit-constr] describes a circuit.
It expands to a circuit constructor.@(lb)
@elemref["circuit-call"]{See above} how to proceed.
@nb{A circuit} can contain multiple instances of identical subcircuits,
but these instances always are distinct objects, without sharing any parts.

@defform-remove-empty-lines[
 @defform[
 (make-circuit-constr name
   (input-wire ...)
   (output-wire ...)
   (sub-output-wires subcircuit) ...)
 #:grammar
 ((input-wire id)
  (output-wire id)
  (sub-input-wire id)
  (sub-output-wire id)
  (subcircuit (subcircuit-constr subcircuit-arg ...))
  (sub-output-wires sub-output-wire (sub-output-wire ...))
  (subcircuit-arg sub-input-wire subcircuit))
 #:contracts
 ((name symbol?)
  (subcircuit-constr circuit-constr?))]{

  At run time each @nbr[subcircuit-constr] is evaluated once only.
  Notice that all operators in a @nbr[subcircuit] are constructors.
  This garantees that distinct instances are made for all @nbr[subcircuit]s.
  For this reason objects like @nbr[Nand] and @nbr[Not] are gate constructors rather than
  the gates themselves.
  The following checks are done during expansion:

  • A duplicate @nbr[input-wire] is not permitted.@(lb)
  • A duplicate @nbr[output-wire] is not permitted.@(lb)
  • A duplicate @nbr[sub-output-wire] is not permitted.@(lb)
  • Every @nbr[input-wire] must appear at least once as a @nbr[sub-input-wire] or an
  @nbr[output-wire].@(lb)
  • Every @nbr[output-wire] must be a @nbr[sub-output-wire] or an @nbr[input-wire].@(lb)
  • Every @nbr[sub-input-wire] must be an @nbr[input-wire] or a @nbr[sub-output-wire].@(lb)
  • Every @nbr[sub-output-wire] must be an @nbr[output-wire] or a @nbr[sub-input-wire] or both.@(lb)
  • A wire cannot be both an @nbr[input-wire] and a @nbr[sub-output-wire].

  The contract @nbr[circuit-constr?] on the @nbr[subcircuit-constr]s is checked
  when the constructor yielded by @nbr[make-circuit-constr] is called.
  A @nbr[subcircuit-constr] can be an arbirary expression yielding a circuit constructor.
  Each @nbr[subcircuit] must have as many inputs as @nbr[subcircuit-arg]s
  and as many outputs as @nbr[sub-output-wire]s.
  Wrong arity is detected when the subcircuit is called for connection to its input and output wires.
  @nb{A @nbr[subcircuit-arg]} that itself is a @nbr[subcircuit]
  can have one output only, for which a @nbrl[report-hidden]{hidden} internal wire is incorporated.
  The name of the hidden wire is that of the @nbr[subcircuit] followed by
  a dot ‘•’ and a natural number. Hidden wires are numbered starting from @nbr[1].
  The counter is increased by 1 for each next hidden wire.
  Subsequent @nbr[make-circuit-constr] forms continue counting from where the previous form ended,
  unless reset by means of procedure @nbr[reset-hidden-cntr!].
  @nb{See parameter} @nbr[report-hidden] too.
  A @nbr[make-circuit-constr] form expands to a circuit constructor:
  @elemtag{circuit}

  @inset{
   @defproc[#:link-target? #f (circuit-constr (wire wire?) ...) void?]}

  A circuit constructor installs a distinct circuit every time it is called.
  It wants n+m @tt{@italic{wire}}s for its arguments,
  the first n corresponding to the @nbr[input-wire]s
  and the last m corresponding to the @nbr[output-wire]s as given to syntax
  @racket[make-circuit-constr]
  and not an @nbr[input-wire] too.
  @nb{It registers} the connections to the input and output wires and
  schedules events in the agenda. No simulation yet.
  Simulation is initiated with procedure @nbr[agenda-execute!].

  All external output wires must be distinct. This is checked.
  Distinct circuits can share input wires,
  but must not share output wires.
  This is not checked.
  Sharing output wires in real life makes no sense,
  for this causes a short circuit
  when one circuit puts signal @nbr[F] on the wire and another one signal @nbr[T].

  @nb{A circuit constructor} must not call itself as a subcircuit, nor directly nor indirectly.
  @nb{An attempt} to do so is detected as an error at run time when the constructor is called.
  Otherwise calling the circuit constructor would lead to an infinite recursion.
  Neither can a real life circuit contain itself as a non trivial subcircuit. 

  @Interaction[
 (define a (make-circuit-constr 'Circuit-a () () (() (a))))
 (a)
 (define b (make-circuit-constr 'Circuit-b () () (() (c))))
 (define c (make-circuit-constr 'Circuit-c () () (() (b))))
 (b)]}]

@defproc[#:kind "predicate"
         (circuit-constr? (obj any/c)) boolean?]{

 Predicates for circuit constructors.

 @Interaction*[
 (define Empty-circuit-constr (make-circuit-constr 'empty-circuit () ()))
 (andmap circuit-constr? (list Not Nand And Empty-circuit-constr))]}

@defproc[(circuit-constr-name (circuit-constr circuit-constr?)) symbol?]{

 Return the name of a circuit constructor.@(lb)
 The name can also be retrieved with procedure @nbr[object-name].

 @Interaction*[
 (map circuit-constr-name (list Not Nand And Empty-circuit-constr))
 (map         object-name (list Not Nand And Empty-circuit-constr))]}

@(reset-Interaction*)

@section[#:tag "gate"]{Gates}

A gate is an elementary circuit.
@nbr[Not], @nbr[Nand] and @nbr[Nand3] gates are the most elementary ones,
or rather their constructors. @nbr[Delay] is a special one.
@nb{All other} predefined gate constructors are built
with @nbr[make-circuit-constr] using @nbr[Not], @nbr[Nand] and @nbr[Nand3] gates
according to the following formulas. See the @seclink["truth tables"]{truth tables} too.

@Tabular[
 (("Gate" "" "Formula") 
  (@tt{(@nbr[And]  @(hspace 2)a b)}   "=" @nbr[(Not (Nand a b))])
  (@tt{(@nbr[Or]   @(hspace 3)a b)}   "=" @nbr[(Nand (Not a) (Not b))])
  (@tt{(@nbr[Nor]  @(hspace 2)a b)}   "=" @nbr[(Not (Nand (Not a) (Not b)))])
  (@tt{(@nbr[Xor]  @(hspace 2)a b)}   "=" @nbr[(Nand (Nand a (Not b)) (Nand (Not a) b))])
  (@tt{(@nbr[Eqv]  @(hspace 2)a b)}   "=" @nbr[(Nand (Nand a b) (Nand (Not a) (Not b)))])
  (@tt{(@nbr[Imply]           a b)}   "=" @nbr[(Nand a (Not b))])
  (@tt{(@nbr[And3] @(hspace 1)a b c)} "=" @nbr[(Not (Nand3 a b c))])
  (@tt{(@nbr[Or3]  @(hspace 2)a b c)} "=" @nbr[(Nand3 (Not a) (Not b) (Not c))])
  (@tt{(@nbr[Nor3] @(hspace 1)a b c)} "=" @nbr[(Not (Nand3 (Not a) (Not b) (Not c)))])
  (@tt{(@nbr[If]   @(hspace 3)a b c)} "=" @nbr[(Nand3 (Nand b c) (Nand a b) (Nand (Not a) c))]))
 #:sep (hspace 2)
 #:row-properties '((top-border bottom-border) () () () () () () () () () bottom-border)
 #:column-properties '(left center left)]

Every gate constructor is a procedure accepting a fixed number of one, two or three input wires
followed by one output wire.

@deftogether[
 (@defproc[#:kind "gate constructor" (Not   (in wire?) (out wire?)) void?]
   @defproc[#:kind "gate constructor" (Nand  (a wire?) (b wire?)           (out wire?)) void?]
   @defproc[#:kind "gate constructor" (Nand3 (a wire?) (b wire?) (c wire?) (out wire?)) void?]
   @defproc[#:kind "gate constructor" (And   (a wire?) (b wire?)           (out wire?)) void?]
   @defproc[#:kind "gate constructor" (Or    (a wire?) (b wire?)           (out wire?)) void?]
   @defproc[#:kind "gate constructor" (Nor   (a wire?) (b wire?)           (out wire?)) void?]
   @defproc[#:kind "gate constructor" (Xor   (a wire?) (b wire?)           (out wire?)) void?]
   @defproc[#:kind "gate constructor" (Eqv   (a wire?) (b wire?)           (out wire?)) void?]
   @defproc[#:kind "gate constructor" (And3  (a wire?) (b wire?) (c wire?) (out wire?)) void?]
   @defproc[#:kind "gate constructor" (Or3   (a wire?) (b wire?) (c wire?) (out wire?)) void?]
   @defproc[#:kind "gate constructor" (Nor3  (a wire?) (b wire?) (c wire?) (out wire?)) void?])]

@defproc[#:kind "gate constructor" (If (test wire?) (then wire?) (else wire?) (out wire?))
         void?]{
 Same as:

 @(hspace 3)@nbr[(Nand3
                   (Nand then else)
                   (Nand test then)
                   (Nand (Not test) else))]

 which is the same as:

 @(hspace 3)@nbr[(Or3
                   (And then else)
                   (And test then)
                   (And (Not test) else))]

 but the former has less delay.
 @nbr[(Nand then else)] is included such as to produce @nbr[T]
 when the @nbr[test] is indeterminate and both @nbr[then] and @nbr[else] are @nbr[T].}

@defproc[#:kind "gate constructor" (Imply (premise wire?) (implication wire?) (out wire?))
         void?]{
 @nbr[Imply] is the same as:

 @(hspace 3)@nbr[(Nand premise (Not implication))]

 which is the same as:

 @(hspace 3)@nbr[(Or (Not premise) implication)]

 but the former has less delay.}

@ignore{
 @deftogether[
 (@defproc[#:kind "procedure" (And* (delay exact-positive-integer?)) circuit-constr?]
   @defproc[#:kind "gate constructor" #:link-target? #f 
            ((And* (delay exact-positive-integer?)) (input wire?) ... (output wire?)) void?])]{
  For an and gate with arbitrary number of @nbr[input]s.@(lb)
  If there is at least one @nbr[input] and at least one of them is @nbr[F],
  the @nbr[output] is @nbr[F].@(lb)
  If all @nbr[input]s are @nbr[T] or there are no @nbr[input]s, the @nbr[output] is @nbr[T].@(lb)
  Else the @nbr[output] is @nbr[?].@(lb)
  The @nbr[output] is delayed with @nbr[delay] units of time.}

 @deftogether[
 (@defproc[#:kind "procedure" (Nand* (delay exact-positive-integer?)) circuit-constr?]
   @defproc[#:kind "gate constructor" #:link-target? #f 
            ((Nand* (delay exact-positive-integer?)) (input wire?) ... (output wire?)) void?])]{
  For a nand gate with arbitrary number of @nbr[input]s.@(lb)
  If there is at least one @nbr[input] and at least one of them is @nbr[F],
  the @nbr[output] is @nbr[T].@(lb)
  If all @nbr[input]s are @nbr[T] or there are no @nbr[input]s, the @nbr[output] is @nbr[F].@(lb)
  Else the @nbr[output] is @nbr[?].@(lb)
  The @nbr[output] is delayed with @nbr[delay] units of time.@(lb)
  @nbr[(Nand* T)] with 2 cq 3 @nbr[input]s does the same as @nbr[Nand] cq @nbr[Nand3].}

 @deftogether[
 (@defproc[#:kind "procedure" (Or* (delay exact-positive-integer?)) circuit-constr?]
   @defproc[#:kind "gate constructor" #:link-target? #f 
            ((Or* (delay exact-positive-integer?)) (input wire?) ... (output wire?)) void?])]{
  For an or gate with arbitrary number of @nbr[input]s.@(lb)
  If there is at least one @nbr[input] and at least one of them is @nbr[T],
  the @nbr[output] is @nbr[T].@(lb)
  If all @nbr[input]s are @nbr[F] or there are no @nbr[input]s, the @nbr[output] is @nbr[F].@(lb)
  Else the @nbr[output] is @nbr[?].@(lb)
  The @nbr[output] is delayed with @nbr[delay] units of time.}

 @deftogether[
 (@defproc[#:kind "procedure" (Nor* (delay exact-positive-integer?)) circuit-constr?]
   @defproc[#:kind "gate constructor" #:link-target? #f 
            ((Nor* (delay exact-positive-integer?)) (input wire?) ... (output wire?)) void?])]{
  For a nor gate with arbitrary number of @nbr[input]s.@(lb)
  If there is at least one @nbr[input] and at least one of them is @nbr[F],
  the @nbr[output] is @nbr[T].@(lb)
  If all @nbr[input]s are @nbr[T] or there are no @nbr[input]s, the @nbr[output] is @nbr[F].@(lb)
  Else the @nbr[output] is @nbr[?].@(lb)
  The @nbr[output] is delayed with @nbr[delay] units of time.}

 @deftogether[
 (@defproc[#:kind "procedure" (Xor* (delay exact-positive-integer?)) circuit-constr?]
   @defproc[#:kind "gate constructor" #:link-target? #f 
            ((Xor* (delay exact-positive-integer?)) (input wire?) ... (output wire?)) void?])]{
  For a xor gate with arbitrary number of @nbr[input]s.@(lb)
  If there is at least one indeterminate @nbr[input], the @nbr[output] is indeterminate.@(lb)
  If all @nbr[input]s are determinate the @nbr[output] is @nbr[F] cq @nbr[T]@(lb)
  if the number of @nbr[input]s with signal @nbr[T] is even cq odd.@(lb)
  The @nbr[output] is delayed with @nbr[delay] units of time.}}

@deftogether[
 (@defproc[#:kind "procedure" (Delay (delay exact-positive-integer?)) circuit-constr?]
   @defproc[#:kind "gate-constructor" #:link-target? #f
            ((Delay (delay exact-positive-integer?)) (in wire?) (out wire?)) void?])]{

 A delay gate has one input and one output equal to the input,@(lb)
 but switching the output is delayed by @nbr[delay] units of time.@(lb)
 Can be used for synchronization.
 See section @seclink["Synchronization"]{Synchronization} for an example.@(lb)
 Can also be used to simulate delays of gates with large fan-out and delays in long wires.

 @Interaction[
 (define in  (wire-make 'in))
 (define out (wire-make 'out))
 ((Delay 10) in out)
 (agenda-sequence! (in (T 2) (F 4) (T 6) (F 8)))
 (parameterize
   ((report-wire-width 3)
    (report-time-width 2)
    (agenda-report #t))
   (agenda-execute!))]}

@section{Logical functions}

A logical function accepts @nbrl[trit?]{trits}
for its arguments and returns one @nbrl[trit?]{trit}.
Some of the functions accept an arbitrary number of arguments,
whereas a gate always has a fixed number of inputs.
@elemtag{generalized-associativity}
Where applicable a note is added about commutativity and associativity.
In this document these two concepts are generalized to functions that can accept more than two
arguments.
Such a function will be said to be commutative if it is invariant
under permutation of its arguments.
It will be said to be associative if a nested application
can be flattened to one single application.
For example, @nbr[And-function] is associative in generalized sense:
@inset{@nbr[(And-function (And-function a b) (And-function c d))]
 can be written as: @nbr[(And-function a b c d)]}
This excludes the distinction between left and right associativity,
but in the present document this distinction is not needed.
A function that is associative when called with two arguments only,
not necessarily is associative in generalized sense too.
For example, the @nbr[Eqv-function]
is associative when restricted to two arguments only,
but not in generalized sense:

@Interaction[
 (for/and ((x (in-list (trit-combinations 3))))
   (define-values (p q r) (apply values x))
   (eq?
     (Eqv-function p (Eqv-function q r))
     (Eqv-function (Eqv-function p q) r)))
 (code:comment #, @red{But:})
 (eq?
   (Eqv-function F F F)
   (Eqv-function F (Eqv-function F F)))]

@defproc[(Not-function (signal trit?)) trit?]{
 @nbr[(Not-function F) = T]@(lb)
 @nbr[(Not-function T) = F]@(lb)
 @nbr[(Not-function ?) = ?]

 Corresponds to a @nbr[Not] gate.}

@defproc[(And-function (signal trit?) ...) trit?]{
 Yields @nbr[T] when called without arguments,@(lb)
 else yields @nbr[F] if at least one @nbr[signal] is @nbr[F],@(lb)
 else yields @nbr[T] if all @nbr[signal]s are @nbr[T],@(lb)
 else yields @nbr[?].

 When called with 2 arguments, the function corresponds to an @nbr[And] gate.@(lb)
 When called with 3 arguments, the function corresponds to an @nbr[And3] gate.@(lb)
 @nbr[And-function] is commutative and associative in generalized sense.}

@defproc[(Nand-function (signal trit?) ...) trit?]{
 Yields @nbr[F] when called without arguments,@(lb)
 else yields @nbr[T] if at least one @nbr[signal] is @nbr[F],@(lb)
 else yields @nbr[F] if all @nbr[signal]s are @nbr[T],@(lb)
 else yields @nbr[?].

 When called with 2 arguments, the function corresponds to a @nbr[Nand] gate.@(lb)
 When called with 3 arguments, the function corresponds to a @nbr[Nand3] gate.@(lb)
 @nbr[Nand-function] is commutative in generalized sense, but not associative.}

@defproc[(Or-function (signal trit?) ...) trit?]{
 Yields @nbr[F] when called without arguments,@(lb)
 else yields @nbr[T] if at least one @nbr[signal] is @nbr[T],@(lb)
 else yields @nbr[F] if all @nbr[signal]s are @nbr[F],@(lb)
 else yields @nbr[?].

 When called with 2 arguments, the function corresponds to an @nbr[Or] gate.@(lb)
 When called with 3 arguments, the function corresponds to an @nbr[Or3] gate.@(lb)
 @nbr[Or-function] is commutative and associative in generalized sense.}

@defproc[(Nor-function (signal trit?) ...) trit?]{
 Yields @nbr[T] when called without arguments,@(lb)
 else yields @nbr[F] if at least one @nbr[signal] is @nbr[T],@(lb)
 else yields @nbr[T] if all @nbr[signal]s are @nbr[F],@(lb)
 else yields @nbr[?].

 When called with 2 arguments, the function corresponds to a @nbr[Nor] gate.@(lb)
 When called with 3 arguments, the function corresponds to a @nbr[Nor3] gate.@(lb)
 @nbr[Nor-function] is commutative in generalized sense, but not associative.}

@defproc[(Xor-function (signal trit?) ...) trit?]{
 Yields @nbr[F] when called without arguments,@(lb)
 else yields @nbr[?] if at least one @nbr[signal] is @nbr[?],@(lb)
 else yields @nbr[T] if an odd number of @nbr[signal]s is @nbr[T],@(lb)
 else yields @nbr[F].

 When called with 2 arguments, the function corresponds to a @nbr[Xor] gate.@(lb)
 Associative and commutative in generalized sense.}

@defproc[(Eqv-function (signal trit?) ...) trit?]{
 Yields @nbr[T] when called without arguments,@(lb)
 else yields @nbr[T] if every @nbr[signal] is @nbr[T] or every @nbr[signal] is @nbr[F],@(lb)
 else yields @nbr[F] if one of the @nbr[signal]s is @nbr[F] and another one @nbr[T],@(lb)
 else yields @nbr[?].

 When called with 2 arguments, the function corresponds to an @nbr[Eqv] gate.@(lb)
 The function is commutative in generalized sense.@(lb)
 It is associative when restricted to two arguments only, but not in generalized sense.}

@defproc[(If-function (test trit?) (then trit?) (else trit?)) trit?]{
 Same as@(lb)
 @tt{(}@nbr[Or-function]@(lb)
 @(hspace 1)@nbr[(And-function then else)]]@(lb)
 @(hspace 1)@nbr[(And-function test then)]@(lb)
 @(hspace 1)@nbr[(And-function (Not-function test) else)]@tt{)}

 Corresponds to an @nbr[If] gate.
 The line @nbr[(And-function then else)] assures that
 a @nbr[T] is produced when the @nbr[test] is indeterminate
 and both @nbr[then] and @nbr[else] are @nbr[T].}

@defproc[(Imply-function (premise trit?) (implication trit?)) trit?]{
 Same as
 @nbr[(Or-function (Not-function premise) implication)]@(lb)
 Corresponds to an @nbr[Imply] gate.}

@defproc[
 ((make-gate*-constr
    (name symbol?)
    (function (trit? ... . -> . trit?)))
  (delay exact-positive-integer?))
 circuit-constr?]{
 Yields a gate constructor that accepts as many inputs as the @nbr[function] accepts trits,
 possibly a variable number of them.
 Such constructors cannot be made with syntax @nbr[make-circuit-constr].
 The gate has one output with a delay of @nbr[delay] units of time.
 @note{Use of procedure @nbr[make-gate*-constr] is discouraged,
  for malicious use may result in gates that share and can mutate each others internal state.
  Therefore it is not mentioned in the @seclink["introduction"]{introduction}.}}

@section[#:tag "truth tables"]{Truth tables}

Lines with determinate inputs only precede all lines @nb{with an} indeterminate input.@(lb)
When all inputs are determinate, the output is determinate too, of course.

@(make-truth-tables (a) (Not Not-function) (Delay values))
@(make-truth-tables (a b) (And And-function)
   (Nand Nand-function)
   (Or Or-function)
   (Nor Nor-function)
   (Xor Xor-function)
   (Eqv Eqv-function)
   (Imply Imply-function))
@(make-truth-tables (a b c) (And3 And-function)
   (Nand3 Nand-function)
   (Or3 Or-function)
   (Nor3 Nor-function)
   (If If-function))

@section[#:tag "More examples"]{More examples}

@subsection[#:tag "Synchronization"]{Synchronization}

A @nbr[Delay] gate can be used for synchronization.
As an example consider an alternative construction of a
@nb{@seclink["D-flip-flop-section"]{D-flip-flop}}.
The new state of a @nb{@seclink["D-flip-flop-section"]{D-flip-flop}} obeys the formula:

@inset{@tt{new-state} = @nbr[(Or (And in clock) (And (Not clock) old-state))]}

which is the same as:

@inset{@tt{new-state =} @nbr[(Nand (Nand in clock) (Nand (Not clock) old-state))]}

Let's try a diagram according to this formula:
@elemtag{another-D-flip-flop}

@inset{@image["another-D-flip-flop.gif" #:scale 1]}

Let's call it @tt{another-D-flip-flop}.
For the moment ignore the triangle in wire @tt{a}.

@Interaction*[
 (define Another-D-flip-flop-constr
   (make-circuit-constr 'Another-D-flip-flop
     (in clock) (state)
     (a     (Nand in clock))
     (b     (Nand state (Not clock)))
     (state (Nand a b))))
 (code:comment "Set the flip-flop.")
 (define-wires
   (wire-in    'in    T)
   (wire-clock 'clock T)
   (wire-state 'state))
 (Another-D-flip-flop-constr wire-in wire-clock wire-state)
 (agenda-report #t)
 (report-wire-width 5)
 (agenda-execute!)]

This seems to work,
but look what happens after dropping the @tt{clock} to @nbr[F]:

@Interaction*[
 (agenda-schedule! wire-clock F)
 (agenda-time-limit 9)
 (agenda-report #t)
 (agenda-execute!)]

@(reset-Interaction*)

There is a mutual dependency between signals @tt{b} and @tt{state}. They oscillate.
This is caused by the fact that signal @tt{b} switches one time step later than signal @tt{a}.
@tt{b} and @tt{state} switch at the same time.
We can repair this by delaying the signal of @tt{a},
in the diagram shown as a triangle (without inversion circle at its output at the right).

@Interaction[
 (define Another-D-flip-flop-constr
   (make-circuit-constr 'Another-D-flip-flop
     (in clock) (state)
     (delayed-a ((Delay 1) (Nand in clock)))
     (b         (Nand state (Not clock)))
     (state     (Nand delayed-a b))))
 (define-wires
   (wire-in 'in T)
   (wire-clock 'clock T)
   (wire-state 'state))
 (Another-D-flip-flop-constr wire-in wire-clock wire-state)
 (agenda-execute!)
 (wire-println wire-state)
 (agenda-schedule! wire-clock F)
 (report-wire-width 9)
 (agenda-report #t)
 (agenda-execute!)
 (wire-println wire-state)]

And now it works.
Notice that the @nbr[Delay] gate is triggered. Its result doesn't matter
because at the same time 6 signal @tt{b} drops to @nbr[F].
Notice also that the signal on wire @tt{b} not necessarily is the inverse of that on wire @tt{state}.

@subsection{Clock}

A @nbr[Delay] gate can be used to make a clock.@(lb)
The one shown below has one input @tt{on/off} and one output @tt{clock}.@(lb)
While @tt{on/off}=@nbr[F], the @tt{clock} remains @nbr[F].@(lb)
While @tt{on/off}=@nbr[T], the @tt{clock} switches after every 10 units of time.

@Interaction[
 (define Clock
   (make-circuit-constr 'Clock (on/off) (clock) 
     (clock (And on/off ((Delay 7) (Not clock))))))
 (code:comment "Delay 7, but And and Not add 3 units of time, total delay 10.")
 (define clock  (wire-make 'clock))
 (define on/off (wire-make 'on/off))
 (Clock on/off clock)
 (report-wire-width 6) 
 (report-time-width 2)
 (agenda-report #t)
 (agenda-sequence! (on/off (F 0) (T 18) (F 69)))
 (agenda-execute!)]

@subsection{Addition}

First a 1 bit adder. A full-adder can be made with two half-adders.@(lb)
Therefore we first define a half-adder:

@(reset-Interaction*)
@Interaction*[
 (define make-half-adder
   (make-circuit-constr 'make-half-adder
     (code:comment "inputs")
     (a b)
     (code:comment "outputs")
     (sum carry-out)
     (code:comment "gates")
     (carry-out (And a b))
     (sum       (And (Or a b) (Not carry-out)))))]

Now we can define the full-adder:

@Interaction*[
 (define make-full-adder
   (make-circuit-constr 'full-adder
     (code:comment "inputs")
     (a b carry-in)
     (code:comment "outputs")
     (sum carry-out)
     (code:comment "subcircuits")
     ((half-sum carry-1) (make-half-adder b carry-in))
     ((sum      carry-2) (make-half-adder a half-sum))
     (carry-out (Or carry-1 carry-2))))]

Let's test the full-adder:

@Interaction*[
 (define wa   (wire-make 'a))
 (define wb   (wire-make 'b))
 (define wc   (wire-make 'c)) 
 (define sum  (wire-make 'sum))
 (define carry-out (wire-make 'carry-out))
 (make-full-adder wa wb wc sum carry-out)]
@Interaction*[
 (for* ((a in-bits) (b in-bits) (c in-bits))
   (agenda-schedule! wa a)
   (agenda-schedule! wb b)
   (agenda-schedule! wc c)
   (agenda-execute!)
   (printf "full-adder a=~s, b=~s, carry-in=~s" a b c)
   (printf " --> sum=~s, carry-out=~s, delay=~s~n"
     (wire-signal sum) (wire-signal carry-out) (sub1 (agenda-time)))
   (agenda-reset!)
   (define result (format "~s ~s" (wire-signal sum) (wire-signal carry-out)))
   (unless
     (case (count T? (list a b c))
       ((0) (equal? result "F F"))
       ((1) (equal? result "T F"))
       ((2) (equal? result "F T"))
       ((3) (equal? result "T T"))
       (else #f))
     (error 'full-adder "~s" (list a b c result))))]

Full adders can be put in a row such as to make an adder for numbers
of more than one bit. We prepare a two's complement 6-bit adder with overflow detection.
It has 13 inputs: 6 for the first operand in order of decreasing significance,
followed by 6 inputs for the second operand in order of decreasing significance
and finally the carry-in bit.
It has 8 outputs: the 6 bits of the sum in order of decreasing significance,
the carry-out bit and an overflow indicator.

@Interaction*[
 (define (make-6-wires name)
   (apply values
     (build-list 6
       (λ (i) (wire-make (string->symbol (format "~s~s" name i)))))))]
@Interaction*[
 (define-values (a0 a1 a2 a3 a4 a5) (make-6-wires 'a))
 (define-values (b0 b1 b2 b3 b4 b5) (make-6-wires 'b))
 (define-values (s0 s1 s2 s3 s4 s5) (make-6-wires 's))
 (define carry-in  (wire-make 'carry-in))
 (define carry-out (wire-make 'carry-out))
 (define overflow  (wire-make 'overflow))]
@Interaction*[
 (define make-6-bit-adder
   (make-circuit-constr '6-bit-adder
     (code:comment "inputs")
     (a5 a4 a3 a2 a1 a0 b5 b4 b3 b2 b1 b0 carry-in)
     (code:comment "outputs")
     (s5 s4 s3 s2 s1 s0 carry-out overflow)
     (code:comment "subcircuits")
     ((s0 c1)        (make-full-adder a0 b0 carry-in))
     ((s1 c2)        (make-full-adder a1 b1 c1))
     ((s2 c3)        (make-full-adder a2 b2 c2))
     ((s3 c4)        (make-full-adder a3 b3 c3))
     ((s4 c5)        (make-full-adder a4 b4 c4))
     ((s5 carry-out) (make-full-adder a5 b5 c5))
     (overflow       (Xor c5 carry-out))))]
@Interaction*[
 (make-6-bit-adder
   a5 a4 a3 a2 a1 a0
   b5 b4 b3 b2 b1 b0
   carry-in
   s5 s4 s3 s2 s1 s0
   carry-out overflow)]

Let's test this 6-bit adder.@(lb)
For this purpose we need conversion between numbers and lists of bits:

@Interaction*[
 (define (number->bit-list n)
   (for/fold ((mask 1) (b '()) #:result b) ((i (in-range 6)))
     (values
       (arithmetic-shift mask 1)
       (cons (if (zero? (bitwise-and mask n)) F T) b))))]
@Interaction*[
 (define (bit-list->number b)
   (for/fold
     ((n 0) (k 1) #:result (if (> n 31) (- n 64) n))
     ((bit (in-list (reverse b))))
     (values
       (if (F? bit) n (+ n k))
       (arithmetic-shift k 1))))]

Let's check @tt{number->bit-list} and @tt{bit-list->number}:

@Interaction*[
 (for/and ((n (in-range -32 32)))
   (= (bit-list->number (number->bit-list n)) n))]
@Interaction*[
 (define (make-bit-list n)
   (define (consF bit-list) (cons F bit-list))
   (define (consT bit-list) (cons T bit-list))
   (cond
     ((zero? n) '(()))
     (else
       (define bit-list (make-bit-list (sub1 n)))
       (append (map consF bit-list)
         (map consT bit-list)))))]
@Interaction*[
 (for/and ((b (in-list (make-bit-list 6))))
   (equal? (number->bit-list (bit-list->number b)) b))]

Now we can do the test on the 6-bit adder:

@Interaction*[
 (for*/and ((n (in-range -32 32)) (m (in-range -32 32)))
   (agenda-reset!)
   (for ((a-wire (in-list (list a5 a4 a3 a2 a1 a0)))
         (a-bit  (in-list (number->bit-list n)))
         (b-wire (in-list (list b5 b4 b3 b2 b1 b0)))
         (b-bit  (in-list (number->bit-list m))))
     (agenda-schedule! a-wire a-bit)
     (agenda-schedule! b-wire b-bit))
   (agenda-schedule! carry-in F)
   (agenda-execute!)
   (define n+m (+ n m))
   (cond
     ((<= -32 n+m 31)
      (=
        (bit-list->number (map wire-signal (list s5 s4 s3 s2 s1 s0)))
        n+m))
     ((T? (wire-signal overflow)))
     (else (error '6-bit-adder "test fails"))))]

@(reset-Interaction*)

One can put 6-bit adders in a row for the preparation of a 12 bit, 18 bit
or any 6n bit adder. In such a row the overflow bit of the 6-bit adders
can be ignored, except that of the most significant one.
An n-bit two's complement adder can be used for numbers in the range
from @tt{-2@↑{n-1}} up to but not including @tt{2@↑{n-1}}.
It can also be used for non-negative numbers
in the range from @nbr[0] up to but not including @tt{2@↑{n}}.
In that case the overflow bit is meaningless and
the carry-out bit being @nbr[T] indicates overflow.

@subsection[#:tag "twin-flip-flop-section"]{Twin-flip-flop}

@note{A twin-flip-flop usually is called a ‘master-slave flip-flop’,
 but that's not a happy name, I think.}
There are several ways to make a twin-flip-flop.
For the present example two SR-flip-flops are used.
Such a flip-flop has three inputs, say @tt{S}, @tt{R} and @tt{clock}.
It has two outputs, say @tt{Q} and @tt{P}
(in the @elemref["D-flip-flop-diagram"]{D-flip-flop} called @tt{state} and @tt{state-inverse})
We first define an SR-flip-flop:

@note{Usually the outputs of a flip-flop are called @tt{Q} and a @tt{Q} with a bar above it.@(lb)
 However, I don't know how to make scribble put a bar above a character.@(lb)
 Read ‘@tt{P}’ as ‘@tt{Q}’ with a bar above it.}

@inset{@image["SR-flip-flop.gif"]}

@Interaction*[
 (define make-SR-flip-flop
   (make-circuit-constr 'SR-flip-flop
     (S R clock) (code:comment "inputs")
     (Q P)       (code:comment "outputs")
     (code:comment "gates")
     (Q (Nand (Nand S clock) P))
     (P (Nand (Nand R clock) Q))))]

The state transition table for an SR-flip-flop after a @nbr[T] pulse on the @tt{clock} is as follows:

@inset{@Tabular[
 (((tt "S") (tt "R") (tt "Q") (tt "P") (list "new "(tt "Q")) (list "new "(tt "P")) "Action/warning")
  ((tt "F") (tt "F") (tt "Q") (tt "P") (tt "Q") (tt "P") "State preserved")
  ((tt "F") (tt "T") (tt " ") (tt " ") (tt "F") (tt "T") "Reset")
  ((tt "T") (tt "F") (tt " ") (tt " ") (tt "T") (tt "F") "Set")
  ((red (tt "T")) (red (tt "T")) " " 'cont 'cont 'cont @red{Do not clock this}))
 #:row-properties '((top-border bottom-border) () () () bottom-border)
 #:column-properties '(()()()() center center left)
 #:sep (hspace 2)]}

A twin-flip-flop has three inputs: @tt{J}, @tt{K} and @tt{clock}.
It has two outputs: @tt{Q} and @tt{P}.@(lb)
The state transition table for a twin-flip-flop after a @nbr[T] pulse on the
@tt{clock} is as follows:

@inset{@Tabular[
 (((tt "J") (tt "K") (tt "Q") (tt "P") (list "new "(tt "Q")) (list "new "(tt "P")) "Action")
  ((tt "F") (tt "F") (tt "Q") (tt "P") (tt "Q") (tt "P") "No change (provided already stable)")
  ((tt "F") (tt "T") (tt " ") (tt " ") (tt "F") (tt "T") "Reset")
  ((tt "T") (tt "F") (tt " ") (tt " ") (tt "T") (tt "F") "Set")
  ((tt "T") (tt "T") (tt "Q") (tt "P") (tt "P") (tt "Q") "Flip (provided already stable)"))
 #:row-properties '((top-border bottom-border) () () () bottom-border)
 #:column-properties '(()()()() 'center 'center 'left)
 #:sep (hspace 2)]}

Twin-flip-flops can be made in several ways.
Here the diagram below is used.@(lb) The two rectangular components are SR-flip-flops.
Don't be confused by the fact that an input or output wire of a subcircuit may be identified by
another identifier than within the circuit the subcircuit is part of.
@nb{In such} case there is one wire only and its name is determined by the outer circuit.

@inset{@image["twin-flip-flop.gif" #:scale 0.75]}

In contrast to an SR-flip-flop, the twin-flip-flop allows clocking with @nb{@tt{J}=@tt{K}=@nbr[T]}
in which case it flips its outputs.
This requires feed back of signals @tt{Q} and @tt{P} to the @tt{S} and @tt{R} inputs
of the first SR-flip-flop.
In order to clock the twin-flip-flop we need two calls,
one with @tt{clock}=@nbr[T] to set or reset or flip the first SR-flip-flop or to leave it as it is and
@nb{one call} with @tt{clock}=@nbr[F] in order to copy the state of the first SR-flip-flop
into the second one.
@tt{clock}=@nbr[T] may change the first SR-flip-flop but leaves the second one unaffected.
@tt{clock}=@nbr[F] leaves the first SR-flip-flop unaffected and
copies the state of the first one into the second one.
Now we can define a constructor for the twin-flip-flop:

@Interaction*[
 (define make-twin-flip-flop
   (make-circuit-constr 'twin-flip-flop
     (J K clock) 
     (Q P)
     (S1      (And J (Nand Q K)))
     (R1      (And K (Nand P J)))
     ((Q1 P1) (make-SR-flip-flop S1 R1 clock))
     ((Q  P ) (make-SR-flip-flop Q1 P1 (Not clock)))))]

Notice that although the diagram of the twin-flip-flop seems complicated,@(lb)
the code for its constructor is rather simple.

@Interaction*[
 (define J     (wire-make 'J))
 (define K     (wire-make 'K))
 (define clock (wire-make 'clock))
 (define Q     (wire-make 'Q))
 (define P     (wire-make 'P))]
@Interaction*[
 (make-twin-flip-flop J K clock Q P)]

Let's test clocking the twin-flip-flop for all @tt{J}, @tt{K} and old state.
First three helper procedures for testing and clocking and for description of the action:

@Interaction*[
 (define (check signal)
   (code:comment "Checks that wires Q and P have the expected signals.")
   (unless
     (and
       (equal? (wire-signal Q) signal)
       (equal? (wire-signal P) (Not-function signal)))
     (error 'twin-flip-flop "test failed")))]

The clock pulse must last long enough to allow@(lb)
the signals to flow through the first @nb{SR-flip-flop}.

@Interaction*[
 (define (clock-the-twin-flip-flop j k)
   (agenda-reset!)
   (agenda-schedule! J     j)
   (agenda-schedule! K     k)
   (agenda-schedule! clock T)
   (agenda-schedule! clock F 10)
   (agenda-execute!))]

@Interaction*[
 (define (action j k old-q)
   (case (format "~s ~s ~s" j k old-q)
     (("F F F") "no action")
     (("F T F") "reset (was already reset)")
     (("T F F") "set")
     (("T T F") "flip")
     (("F F T") "no action")
     (("F T T") "reset")
     (("T F T") "set (was already set)")
     (("T T T") "flip")))]

Now the tests:

@Interaction*[
 (for* ((j in-bits) (k in-bits) (old-q in-bits))
   (code:comment "First put the twin-flip-flop in state Q=old-q.")
   (clock-the-twin-flip-flop old-q (Not-function old-q))
   (code:comment "Check that the flip-flop is initialized properly:")
   (check old-q)
   (code:comment "Now clock the flip-flop with J=j and K=k.")
   (clock-the-twin-flip-flop j k)
   (code:comment "Check the results.")
   (check
     (Or-function
       (And-function j (Not-function old-q))
       (And-function old-q (Not-function k)))) 
   (code:comment "Show results.")
   (printf
     "J=~s, K=~s, old-Q=~s --> ~a, ~a~n"
     j k old-q
     (wire-println #:port 'string #:sep ", " Q P)
     (action j k old-q)))]

@(reset-Interaction*)

@section{Exact timing?}

The simulators described in the present document,
change signals instantaneously and at exact times.
In real life circuits this is not the case.
I gladly leave the details to experts with more knowledge about electronic circuits.
I suspect that the simulators are of little use in practice.
At best they can be used for preliminary tests of correctness.
Anyway, @nb{I hope} my module can be a nice toy for some of you.

@bold{@larger{The end}}
