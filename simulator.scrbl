#lang scribble/manual

@;════════════════════════════════════════════════════════════════════════════════════════════════════

@(require
   "scribble-utensils.rkt"
   racket
   "simulator.rkt"
   (for-syntax)
   (for-label racket "simulator.rkt")
   (for-template "simulator.rkt"))

@title{CPU simulator}
@author{Jacob J. A. Koot}

@(Defmodule)
Related files:
@inset{@Tabular[
 ((@nbhll["simulator.rkt"]{simulator.rkt} "Source code of the simulator")
  (@nbhll["examples.rkt"]{examples.rkt} "")
  (@nbhll["tests.rkt"]{tests.rkt} (list "Requires "@url["https://github.com/joskoot/test"])))
 #:sep (hspace 2)]}

@section{Introduction}

The CPU simulator described in the present document @nbrl[execute]{executes} a binary coded program
that can be made with its @nbsl["sec-assembler"]{assembler}.
Every instruction consists of one word.
Execution of an instruction is done by a micro-program consisting of one or two micro-instructions,
one if the instruction does not access memory, in all other cases two.
This means that an instruction that does not access memory takes one clock cycle,
the next instruction being read within the same cycle.
When the instruction accesses memory,
the next instruction is read during an additional cycle.
Direct access input to memory from a port and output from memory to a port is implemented.
They delay the additional cycle for reading the next instruction
until the input or output is complete.
Memory caches, memory banking and virtual memory are not simulated.
The instruction set is defined in a table.
Both the @nbrl[assemble]{assembler} and the
@nbrl[execute]{simulator} are driven by this table.
See variable @tt{mnemonics} in the @nbhll["simulator.rkt"]{source-code}.

@Tabular[
 (("Notation" 'cont 'cont)
  ("n .. m" ":" "from n up to but not including m.")
  ("n :: m" ":" "from n up to and including m."))
 #:sep (hspace 1)]

@section{Definitions}

@Tabular[
 (("Word"
   @roman{Consists of 64 bits, counted 0..64 in order of increasing significance.})
  ("Address"
   @roman{Consists of 20 bits, counted 0..20 in order of increasing significance.})
  ("Datum"
   @roman{Consists of 44 bits, counted 0..44 in order of increasing significance.})
  (@nbsl["sec-registers"]{Register}
    @roman{Contains a word or an address and is clocked when selected.@(lb)
  At clock-up time it accepts its input without altering its output.@(lb)
  At clock-drop it transfers its input to its output.})
  (@nbsl["sec-circuits"]{Circuit}
    @roman{Has one or more inputs and one output. Not clocked.@(lb)
  The output is a function of the inputs.@(lb)
  When the circuit is selected the output is available within clock-up period.@(lb)})
  (@nbsl["sec-memory"]{Memory}
    @roman{Contains 2@↑{20} words.})
  (@nbsl["sec-alu"]{@tt{@bold{ALU}}} @roman{Arithmetic unit.})
  (@nbsl["sec-cmp"]{@tt{@bold{CMP}}} @roman{Arithmetic comparison unit, used for conditional jumps.})
  (@nbsl["sec-instruction-register"]{@tt{@bold{IR}}} @roman{Instruction register.}))
 #:sep (hspace 2)
 #:row-properties '((top top-border bottom-border) (top bottom-border))]

It would be nice to have a memory of more than 2@↑{20} words.
In principle 20 can be increased to 44
resulting in an address space of a more than 17×10@↑{12} words or 140×10@↑{12} bytes,
but as the simulator keeps memory in a vector,
this makes the @nbrl[assemble]{assembler} and procedure @nbr[execute] use more words
than acceptable for DrRacket or Racket
or available in your computer.
Memory caches and banking would slow down the simulation.
Virtual memory in a file with a limited number of pages in RAM
would slow down the simulation too,
even without page faults,
but would allow a larger address space.
Anyway, you wouldn't want to use the simulator for programs of more than a million words.

@section[#:tag "sec-instruction-register"]{Instruction register}

Instruction register @tt{@bold{IR}} contains a word in which the components
listed below are discerned.
The length of every component is a multiple of 4, even when less bits are required.
This makes reading hexadecimally printed instructions easier. 

@Tabular[
 (("name" "bits" "length" "description")
  ("opcode" "56 .. 64" "8 bits" "instruction code")
  ("Ra"     "52 .. 56" "4 bits" "register designator")
  ("Rb"     "48 .. 52" "4 bits" "register designator")
  ("Rc"     "44 .. 48" "4 bits" "register designator")
  ("datum"  "00 .. 44" "44 bits" "datum"))
 #:row-properties '((top-border bottom-border) ()()()() bottom-border)
 #:column-properties '(left left right left)
 #:sep @(hspace 2)]

The opcode determines which ones of the other components are relevant.
A selected register designator is @nbr[#x0]::@nbr[#x7] for a register
or @nbr[#x8] for the sign extended datum part of the @tt{@bold{IR}}.

Most processors include instructions smaller than a whole word and
pack short instructions together in one word. This has two advantages:
first, it reduces memory consumption,
second, when a memory cycle is much longer than a CPU cycle, it speeds up.
All of the instructions of the CPU described in the present document
take a whole word. This simplifies the simulator without making it slower,
because reading an instruction from memory is simulated as fast or even faster than
decomposing a word into its short instructions.
This would require a more complicated micro-processor.

@section[#:tag "sec-registers"]{Registers}

@Tabular[
 (("name" "size" "usage" "register designator?" "initial value")
  (@roman{@tt{@bold{R0}} :: @tt{@bold{R7}}} "word" "general purpose" "yes" "0")
  (@tt{@bold{SP}} "address" "stack pointer"        "no" @roman{2@↑{20}−1})
  (@tt{@bold{PC}} "address" "program counter"      "no" "0")
  (@tt{@bold{IR}} "word"    "instruction register" "no" "na"))
 #:sep (hspace 2)
 #:row-properties '((top-border bottom-border) ()()() bottom-border)]

Register designators can appear explicitly in assembler code.
@tt{@bold{SP}}, @tt{@bold{PC}} and @tt{@bold{IR}} are used implicitly
depending on the operation code in the @tt{@bold{IR}}.

@section{Clock}
The simulator has a virtual clock.
Virtually registers and circuits are triggered simultaneously.
Registers are clocked.
@nb{At clock} raise they accept their inputs without altering their outputs.
@nb{At clock} drop they transfer their inputs to their outputs.
@nb{An instruction} is executed by first triggering the involved registers for clock raise
and subsequently triggering them for clock drop.
Memory, when activated, starts reading or writing at clock-raise
and finishes reading before clock-drop and writing within the clock-cycle.
Other elements are not clocked.
When their inputs change the possible mutation of their outputs is accomplished before clock drop.

@section[#:tag "sec-memory"]{Memory}

@Tabular[
 (("name" "size" "in/out" "description")
  (@tt{@bold{MA}} "address" "input"
    @roman{memory address})
  (@tt{@bold{MW}} "word" "input"
    @roman{word to be written in memory at address @tt{@bold{MA}}})
  (@tt{@bold{MR}} "word" "output"
    @roman{word read from memory at address @tt{@bold{MA}}}))
 #:sep (hspace 2)
 #:row-properties '((top-border bottom-border) ()() bottom-border)]

When @tt{@bold{MW}} is selected,
memory takes the word to be written and address @tt{@bold{MA}} at clock raise time and
starts writing immediately.
When @tt{@bold{MR}} is selected, memory takes address @tt{@bold{MA}} at clock-raise time,
starts reading immediately and delivers the read word before clock-drop.
Reading from and writing to memory cannot be done within the same clock cycle.
@nb{The memory} is not divided in independent banks.
This means that never more than one word can be written to or read from memory simultaneously.

@section[#:tag "sec-alu"]{Arithmetic unit}

The arithmetic unit @tt{@bold{ALU}} has two word inputs: @tt{@bold{A1}} and @tt{@bold{A2}}.
It has one word output @tt{@bold{AO}}.
Overflow is ignored. Arithmetic instructions take one clock-cycle, or rather,
clock drop is delayed until the @tt{@bold{ALU}} has produced its output.
For example, a division may take more time than a bitwise logical operation.
No floating point instructions are implemented.

@section[#:tag "sec-cmp"]{Compare unit}
The arithmetic compare unit @tt{@bold{CMP}} wants one or two word inputs:
@tt{@bold{C1}} and @tt{@bold{C2}}.
When @tt{@bold{C2}} is ommitted @tt{@bold{C1}} is compared to zero.
It is used in conditional jump operations.
If the comparator yields false, it inhibits the jump.
The comparison is made for two's complement words and is not faulted by overflow.
Jumps, both unconditional ones and both accepted or inhibited conditional ones take one clock-cycle.
As the micro-processor has no instruction cash
and does nothing else than selecting registers and circuits to be clocked,
there is no read ahead and no speculative execution of subsequent instructions
before finishing the current one.
For most instructions, all jumps included,
the micro-processor executes one micro-instruction and fetches the next instruction from memory
during the same clock cycle as the one currently being executed.
In all other cases two micro-instructions are needed,
one for the instruction proper and one for fetching the next instruction.

@section[#:tag "sec-circuits"]{Circuits}

Circuits are not clocked. They provide their outputs without waiting for clock-drop.

@Tabular[
 (("name" "size" "in/out" "description")
  (@tt{@bold{P+}} "address" "output"
    @roman{@tt{@bold{PC}}+1 modulo 2@↑{20}})
  (@tt{@bold{S+}} "address" "output"
    @roman{@tt{@bold{SP}}+1 modulo 2@↑{20}})
  (@tt{@bold{S-}} "address" "output"
    @roman{@tt{@bold{SP}}−1 modulo 2@↑{20}})
  (@tt{@bold{DA}} "word" "output"
    @roman{the sign extended datum part of the @tt{@bold{IR}}})
  (@tt{@bold{A1}} "word" "input"
    @roman{@tt{@bold{ALU}} input})
  (@tt{@bold{A2}} "word" "input"
    @roman{@tt{@bold{ALU}} input})
  (@tt{@bold{AO}} "word" "output"
    @roman{@tt{@bold{ALU}} output})
  (@tt{@bold{C1}} "word" "input"
    @roman{@tt{@bold{CMP}} input})
  (@tt{@bold{C2}} "word" "input"
    @roman{@tt{@bold{CMP}} input (zero if omitted)}))
 #:sep (hspace 2)
 #:row-properties '((top-border bottom-border) ()()()()()()()() bottom-border)]

@section[#:tag "sec-busses"]{Busses}

Virtually, the simulated processor does nothing else than clocking busses between
registers, circuits, memory and the input and output controller.
There are sixteen busses, three switchable ones and thirteen fixed ones.
They can transfer their signals simultaneously during the same clock cycle.
The input of a bus is called its `entrance´ and its output its `exit´.
The opcode in the @tt{@bold{IR}} controls which register or circuit output is connected
to the entrance of a switchable bus and to which register or circuit input its exit is connected.
These switches are marked as ‘↑’.
A fixed bus always has the same register or circuit output to its entrance and the same
register or circuit input from its exit.
Four of the fixed busses, marked by ‘f’, always are open.
The other nine, marked by ‘s’, are open during clock up period when selected by the opcode.
When a datum is transferred to a word register it is sign extended.
When an address is transferred to a word-register it is extended with zero bits
at the high significant end.
When a word or a datum is transferred to an address register
it is truncated to its 20 lower significant bits.

@elemtag{bus-table}
@Tabular[
 (("from↓ to→" @tt{@bold{Rn}} @tt{@bold{SP}} @tt{@bold{S+}} @tt{@bold{S-}} @tt{@bold{PC}}
               @tt{@bold{P+}} @tt{@bold{A1}} @tt{@bold{A2}} @tt{@bold{C1}} @tt{@bold{C2}}
               @tt{@bold{MW}} @tt{@bold{MA}} @tt{@bold{IR}} @tt{@bold{DA}})
  @;              Rn  SP  S+  S-  PC  P+  A1  A2  C1  C2  MW  MA  IR  DA  
  (@tt{@bold{Rn}} "↑" " " " " " " " " "↑" "↑" "↑" "↑" "↑" "↑" "↑" " " " ")
  (@tt{@bold{SP}} " " " " "f" "f" " " " " " " " " " " " " " " "s" " " " ")
  (@tt{@bold{S+}} " " "s" " " " " " " " " " " " " " " " " " " " " " " " ")
  (@tt{@bold{S-}} " " "s" " " " " " " " " " " " " " " " " " " "s" " " " ")
  (@tt{@bold{PC}} " " " " " " " " " " "s" " " " " " " " " " " "s" " " " ")
  (@tt{@bold{P+}} " " " " " " " " "f" " " " " " " " " " " " " " " " " " ")
  (@tt{@bold{AO}} "↑" " " " " " " " " " " " " " " " " " " " " " " " " " ")
  (@tt{@bold{MR}} "↑" " " " " " " " " " " " " " " " " " " " " " " "s" " ")
  (@tt{@bold{IR}} " " " " " " " " " " " " " " " " " " " " " " " " " " "f")
  (@tt{@bold{DA}} "↑" " " " " " " " " "s" " " " " " " " " " " "s" " " " "))
 #:sep "│"
 #:column-properties '(center)
 #:row-properties '((top-border bottom-border) bottom-border)]

A bus exit can be connected to more than one input at the same time,
but a bus entrance can be connected to one output only at the same time.
This means that in every column only one ‘↑’ or ‘s’ can be open at the same time.
Because there are three switchable busses only, not more than three connections ‘↑’
of the whole table can be selected at the same time.
The ‘s’ busses @tt{@bold{PC}} → @tt{@bold{MA}} and @tt{@bold{MR}} → @tt{@bold{IR}}
make it possible to read the next instruction during the same clock cycle as the operation proper,
provided the input of @tt{@bold{MA}} is not connected to the exit of another bus
and the output of @tt{@bold{MR}} is not connected to the entrance of another bus,
otherwise an additional cycle is needed to read the next instruction.
@nb{A jump} is made by selecting the busses @tt{@bold{Rn}/@bold{DA}} → @tt{@bold{MA}}
and @tt{@bold{MR}} → @tt{@bold{IR}}
and adjusting the @tt{@bold{PC}} with bus @tt{@bold{Rn}/@bold{DA}} → @tt{@bold{P+}}.
The f bus @tt{@bold{P+}} → @tt{@bold{PC}} assures that the @tt{@bold{PC}}
always to points to the memory word following the word from which the last instruction was read.

@section{Input/output ports}

Two ports are simulated,
one for @nbrl[INP-port]{input} and one for @nbrl[OUT-port]{output}.
They support direct access to and from memory,
are clocked by the input cq. output device
and have their own busses from and to memory
(not included in the @elemref["bus-table"]{above table}).
Dropping of the CPU clock is delayed until input cq. output
has been completed. The next instruction is read after completion
by means of an additional CPU cycle.

@section[#:tag "sec-assembler"]{Assembler}

‘@tt{Ra}’, ‘@tt{Rb}’ and ‘@tt{Rc}’ are register designators: @tt{R0} :: @tt{R7}. @(lb)
An instruction has the form
@inset{@tt{(opcode-mnemonic }detail@tt{ ...)}}
It may be given a name for its address by writing
@inset{@nb{@tt{(address : opcode-mnemonic @roman{detail} ...)}}}
where @tt{address} is a symbol other than a register designator or colon.@(lb)
All @tt{address}es must be distinct.
Comments can be inserted as:
@inset{@tt{(: @roman{text of the comment})}}
@nb{All elements} of an instruction must be separated by blank space.
‘@tt{d}’ must be an exact integer or an @tt{address}.
If it is an exact integer it is truncated to its 44 lower significant bits.
If it is an @tt{address} it is extended to a datum by adding zero bits at the high-significant end.
‘@tt{Rd}’ is ‘@tt{Ra}’, ‘@tt{Rb}’, ‘@tt{Rc}’ or a @tt{d}.
@nb{A ‘@tt{w/a}’} can occur in @tt{DATUM} and @tt{DATA} declarations.
It must be an exact integer or an @tt{address}. 
If it is an integer it is truncated to the 64 lower significant bits.
When it is an @tt{address}
it is extended to a word by adding zero bits at the high significant end.
Arithmetical operations are signed in two's complement.
Overflow is ignored.
No floating point instructions are implemented.

@(define (~op e)
   (tt
     (format "~a"
       (string-upcase (~r (mnemonic->opcode e) #:min-width 2 #:base 16 #:pad-string "0")))))

@tabular[
 (cons
   (list "Hexadecimal opcode, instruction and description" 'cont 'cont)
   (append
     (sort
       (list
         (list @(~op 'STP)
           @nb{@tt{(STP)}} @roman{Halts the processor.})
         
         (list @(~op 'NOP)
           @nb{@tt{(NOP)}} @roman{No operation.})
         
         (list @(~op 'SET)
           @nb{@tt{(SET Ra Rd)}} @roman{@tt{Ra} ← @tt{Rd}.})
         
         (list @(~op 'ADD)
           @nb{@tt{(ADD Ra Rb Rd)}} @roman{@tt{Ra} ← @tt{Rb+Rd}.})
         
         (list @(~op 'SUB)
           @nb{@tt{(SUB Ra Rb Rd)}} @roman{@tt{Ra} ← @tt{Rb}−@tt{Rd}.})
         
         (list @(~op 'MUL)
           @nb{@tt{(MUL Ra Rb Rd)}} @roman{@tt{Ra} ← @tt{Rb}×@tt{Rd}.})
         
         (list @(~op 'DIV)
           @nb{@tt{(DIV Ra Rb Rd)}} @roman{@tt{Ra} ← @tt{Rb/Rd}, integer division
  (crash if @tt{Rd} is zero)})
         
         (list @(~op 'REM)
           @nb{@tt{(REM Ra Rb Rd)}} @roman{@tt{Ra} ← remainder of @tt{Rb/Rd},
  (crash if @tt{Rd} is zero)})
         
         (list @(~op 'MOD)
           @nb{@tt{(MOD Ra Rb Rd)}}
           @roman{@tt{Ra} ← @tt{Rb} modulo @tt{Rd} (crash if @tt{Rd} is zero)})
         
         (list @(~op 'SHL)
           @nb{@tt{(SHL Ra Rb Rd)}} @roman{Put @tt{Rb} into @tt{Ra},
  left shifted by @tt{Rd} bits.})
         
         (list @(~op 'SHR)
           @nb{@tt{(SHR Ra Rb Rd)}} @roman{Put @tt{Rb} into @tt{Ra},
  right shifted by @tt{Rd} bits, no sign extension.})
         
         (list @(~op 'SHE)
           @nb{@tt{(SHE Ra Rb Rd)}} @roman{Put @tt{Rb} into @tt{Ra},
  right shifted by @tt{Rd} bits and sign extended.})
         
         (list @(~op 'ROT)
           @nb{@tt{(ROT Ra Rb Rd)}} @roman{Put @tt{Rb} into @tt{Ra},
  rotated by @tt{(@nbr[modulo] Rd 64)} bits to the left.})
         
         (list @(~op 'AND)
           @nb{@tt{(AND Ra Rb Rd)}} @roman{@tt{Ra} ← @tt{Rb˄Rd}, bitwise and.})
         
         (list @(~op 'IOR)
           @nb{@tt{(IOR Ra Rb Rc)}} @roman{@tt{Ra} ← @tt{Rb˅Rd}, bitwise inclusive or.})
         
         (list @(~op 'NEG)
           @nb{@tt{(NEG Ra Rd)}} @roman{@tt{Ra} ← −@tt{Rd}.})
         
         (list @(~op 'NOT)
           @nb{@tt{(NOT Ra Rd)}} @roman{@tt{Ra} ← @tt{@roman{¬}Rd}, bitwise not.})
         
         (list @(~op 'JMP)
           @nb{@tt{(JMP Rd)}} @roman{Jump to @tt{Rd}.})
         
         (list @(~op 'EQ?)
           @nb{@tt{(EQ? Ra Rb Rd)}} @roman{If @tt{Ra}=@tt{Rb} jump to @tt{Rd}
  else go to next instruction.})
         
         (list @(~op 'LT?)
           @nb{@tt{(LT? Ra Rb Rd)}} @roman{If @tt{Ra}<@tt{Rb} jump to @tt{Rd}
  else go to next instruction.})
         
         (list @(~op 'GT?)
           @nb{@tt{(GT? Ra Rb Rd)}} @roman{If @tt{Ra}>@tt{Rb} jump to @tt{Rd}
  else go to next instruction.})
         
         (list @(~op 'LE?)
           @nb{@tt{(LE? Ra Rb Rd)}} @roman{If @tt{Ra}≤@tt{Rb} jump to @tt{Rd}
  else go to next instruction.})
         
         (list @(~op 'GE?)
           @nb{@tt{(GE? Ra Rb Rd)}} @roman{If @tt{Ra}≥@tt{Rb} jump to @tt{Rd}
  else go to next instruction.})
         
         (list @(~op '=0?)
           @nb{@tt{(=0? Ra Rd)}} @roman{If @tt{Ra}=0 jump to @tt{Rd}
  else go to next instruction.})
         
         (list @(~op '<0?)
           @nb{@tt{(<0? Ra Rd)}} @roman{If @tt{Ra}<0 jump to @tt{Rd}
  else go to next instruction.})
         
         (list @(~op '>0?)
           @nb{@tt{(>0? Ra Rd)}} @roman{If @tt{Ra}>0 jump to @tt{Rd}
  else go to next instruction.})
         
         (list @(~op '≤0?)
           @nb{@tt{(≤0? Ra Rd)}} @roman{If @tt{Ra}≤0 jump to @tt{Rd}
  else go to next instruction.})
         
         (list @(~op '≥0?)
           @nb{@tt{(≥0? Ra Rd)}} @roman{If @tt{Ra}≥0 jump to @tt{Rd}
  else go to next instruction.})
         
         (list @(~op 'INP)
           @nb{@tt{(INP Ra)}} @roman{Reads a word from @nbr[INP-port] and puts it in Ra.})
         
         (list @(~op 'OUT)
           @nb{@tt{(OUT Rde)}} @roman{Print @tt{Rd} on @nbr[OUT-port].})
         
         (list @(~op 'RÆD)
           @nb{@tt{(RÆD Ra Rd)}}
           @roman{Direct access input to addresses from Ra .. Ra+Rd})
         
         (list @(~op 'WRT)
           @nb{@tt{(WRT Ra Rd)}}
           @roman{Direct access output from addresses from Ra .. Ra+Rd})
         
         (list @(~op 'PSH)
           @nb{@tt{(PSH Rd)}} @roman{Push @tt{Rd} onto the stack.})
         
         (list @(~op 'POP)
           @nb{@tt{(POP Ra)}} @roman{Pop a word from stack into @tt{Ra}.})
         
         (list @(~op 'MWR)
           @nb{@tt{(MWR Ra Rd)}} @roman{Write @tt{Ra} in memory at address @tt{Rd}.})
         
         (list @(~op 'MRD)
           @nb{@tt{(MRD Ra Rd)}}
           @roman{Read memory at address @tt{Rd} and put the word in @tt{Ra}.}))
       
       string<? #:key (λ (e) (car (element-content (car e)))))
     (list
       (list " " @nb{@tt{(DATUM w/a)}@larger{@(hspace 1)}} 'cont)
       (list " " @nb{@tt{(DATA w/a} ...@tt{)}} @roman{Expanded to @tt{(DATUM w/a)}  ... .})
       (list " " @nb{@tt{(: @roman{comment ...)}}} @roman{Ignored.}))))
     
 #:sep (hspace 2)
 #:row-properties
 (append
   '((top top-border bottom-border))
   (make-list 35 'top)
   '((top bottom-border))
   '(bottom (bottom bottom-border)))]

The @tt{PSH} and @tt{POP} instructions use the @tt{@bold{SP}} for addressing.
When pushing, address @tt{@bold{SP}} is used and @tt{@bold{SP}} is decreased by one.
When popping @tt{@bold{S+}} is used as address and @tt{@bold{SP}} is increased by one.
At the start of execution @tt{@bold{SP}} is 2@↑{20}−1.
@tt{PSH} and @tt{POP} instructions should be balanced like parentheses.
Instructions @tt{WRT} and @tt{RÆD} use direct access to memory.
They take as many port cycles as words read or written
plus a CPU cycle to read the next instruction.
@tt{SHL}, @tt{SHR} and @tt{SHE} take the shift counts as signed.
A negative shift-count for @tt{SHL} effectively does @tt{SHR} and reversely.
A negative shift count for @tt{SHE} effectively does @tt{SHL}.

@Interaction[
 (parameterize ((show #f))
   (execute
     `((SET R1 ,(hex "#xFFFFFFFF0000FFFF"))
       (OUT R1)
       (SHE R2 R1 8)
       (SHE R3 R1 -8)
       (OUT R2)
       (OUT R3))))]
                                                                 
@section{Provided}

@defproc[
 (assemble
   (‹instrs› (listof #, @nbsl["sec-assembler"]{instruction}))
   (#:offset ‹offset› natural? 0)
   (#:target ‹target› (or/c 'reset 'memory 'list) 'reset))
 void?]{
 @nbsl["sec-assembler"]{Assembles} the instructions.
 Addresses are offsetted according to @nbr[‹offset›].
 If @nbr[‹target›] is @nbr['reset] memory is cleared and
 the instructions are put in memory starting from address @nbr[‹offset›].
 @nb{If @nbr[‹target›]} is @nbr['memory]
 the instructions are put in memory starting from address @nbr[‹offset›] but
 without clearing memory first.
 @nb{If @nbr[‹target›]} is @nbr['list]
 the code is returned as a list: @nb{@tt{((address instr) ...)}}.

 @Interaction[
 (define binary-code
   (parameterize ((show #f))
     (assemble
       '((aap  : NOP)
         (noot : ADD R0 R1 R2)
         (mies : STP)
         (wim  : DATUM wim)
         (zus  : DATA aap noot mies wim zus -1 +1))
       #:offset 5
       #:target 'list)))
 (for ((instr (in-list binary-code)))
   (printf "~a : ~a~n" (~ax (car instr)) (~wx (cdr instr))))]}

@defproc[(execute
           (‹instrs› (or/c #f (listof #, @nbsl["sec-assembler"]{instruction})) #f)
           (#:start ‹start› natural? 0)
           (#:offset ‹offset› natural? 0)) void?]{ 
 Resets all registers and executes the program currently in memory starting at address
 @nb{@tt{@bold{PC}} = @nbr[‹start›]}.
 @nb{If @nbr[‹instrs›]} is a list of instructions,
 the @nbrl[assemble]{assembler} is called first as
 @nbr[(assemble ‹instrs› #:offset ‹offset› #:target 'reset)].@(lb)
 @nb{If @nbr[‹instrs›]} is @nbr[#f], the @nbr[‹offset›] is ignored.}

@defparam*[show ‹options›
 (or/c #f 'all
   (listof
     (or/c
       'source-code
       'binary-code
       'instructions
       'registers)))
 (listof
   (or/c
     'source-code
     'binary-code
     'instructions
     'registers))
 #:value '(source-code binary-code instructions registers)]{
 @nbr[(show 'all)] activates all options.@(lb)
 @nbr[(show #f)] and @nbr[(show '())] disable all options.

 @itemlist[
 @item{When this parameter contains option @nbr['source-code],@(lb)
   the @nbrl[assemble]{assembler} shows the program to be assembled.}
 
 @item{When this parameter contains option @nbr['binary-code],@(lb)
   procedure @nbr[assemble] shows the assembled binary code.}

 @item{When this parameter contains option @nbr['instructions],@(lb)
   procedure @nbr[execute] prints all executed instructions:
   @inset{@blue{@tt{line-nr} @tt{:} @tt{mnemonic}
     @tt{:}
     @tt{opcode} @tt{ra} @tt{rb} @tt{rc} @tt{datum} @tt{:}
     @tt{address} @tt{:} @tt{cycle-count}}}
   The elements are printed in hexadecimal form,@(lb)
   the line-nr, mnemonic and cycle-count excepted.@(lb)
   The cycle count is accumulative and includes both CPU and port cycles.@(lb)
   In fact the count is the number of words read from or written to memory.}

 @item{When this parameter contains option @nbr['registers],@(lb)
   procedure @nbr[execute] shows the contents
   of registers after completion of the program.}]}

@defparam[align ‹n› exact-nonnegative-integer? #:value 3]{
 In a print of the executed instructions as indicated by parameter @nbr[show],
 each line begins with a line number.
 This number is right aligned in a field of @tt{(@nbr[align])} digits.
 Line numbers requiring more digits are not truncated.}

@defparam[OUT-port ‹port› output-port? #:value (current-output-port)]{
 Parameter specifying on which port instructions @tt{WRT} and @tt{OUT} print output.}

@defparam[INP-port ‹port› output-port? #:value (current-input-port)]{
 Parameter specifying from which port instructions @tt{INP} and @tt{RÆD} read input.
 @Interaction[
 (parameterize
   ((INP-port (open-input-string "#x0123456789abcdef")))
   (execute '((INP R0) (NOT R1 R0))))]
 @Interaction[
 (show '(instructions))
 (INP-port (open-input-string "1 2 3 4 5"))
 (assemble
   '((SET R0 data)
     (RÆD R0 5)
     (STP)
     (data : DATA)))
 (print-memory 3 5)
 (execute)
 (print-memory 3 5)]}

@defparam*[catch-crash ‹yes/no› any/c boolean? #:value #f]{
 In case of a crash procedure @nbr[execute]
 prints a message on the @nbr[current-error-port] and halts by raising an exception.
 If this parameter is set to a true value,
 this exception is catched and procedure @nbr[execute] returns normally.

 @Interaction[
 (parameterize ((catch-crash #t))
   (execute '((DATUM -1)))
   (displayln "crash has been catched"))]}

@defproc[(print-memory (‹n› exact-nonnegative-integer?) (‹m› exact-nonnegative-integer?)) void?]{
 Prints words from addresses @nbr[‹n›] .. @nbr[‹n›]+@nbr[‹m›] of memory.@(lb)
 Each word is preceded by its hexadecimal address.

 @Interaction[
 (parameterize ((show #f))
   (assemble '((DATA 1 2 3 -1 -2 -3))))
 (print-memory 0 6)]}

@defproc[(print-stack (‹n› exact-nonnegative-integer?)) void?]{
 Shows the last @nbr[‹n›] words of memory, where the stack is located.@(lb)
 Each word is preceded by its hexadecimal address.

 @Interaction[
 (parameterize ((show #f))
   (execute
     '((PSH 1)
       (PSH 2)
       (PSH 3)
       (POP R1)
       (POP R2)
       (POP R3)
       (OUT R1)
       (OUT R2)
       (OUT R3))))
 (print-stack 5)]}

@defproc[(print-registers) void?]{
 Prints registers @nbr[R0] :: @nbr[R7],
 @nbr[SP] and @nbr[PC].}

@defproc[(memory-ref (‹addr› natural?)) natural?]{
 The @nbr[‹addr›] is truncated to the lower 20 bits.}

@defproc[(memory-set! (‹addr› natural?) (‹word› exact-integer?)) void?]{
 The @nbr[‹addr›] is truncated to the lower 20 bits.@(lb)
 The @nbr[‹word›] is truncated to the lower 64 bits.

 @Interaction[
 (memory-set! (expt 2 26) -10)
 (print-memory 0 1)]}

@defparam[max-nr-of-instrs ‹n› exact-nonnegative-integer? #:value 1000]{
 Puts a limit on the number of instructions to be executed by procedure @nbr[execute].@(lb)
 The procedure halts when the limit is exceeded.

 @Interaction[
 (parameterize ((max-nr-of-instrs 5) (show '(instructions)))
   (execute '((loop : JMP loop))))]}

@defproc[(reset) void?]{
 Clears memory and resets registers.}

@Elemtag{Rx}
@defproc[#:link-target? #f (Rx (‹arg› (or/c #f exact-integer? 'clock) #f))
         exact-nonnegative-integer?]{
 @nbpr{Rx} is one of the following:}
@inset{@deftogether[
 ((defidform #:kind "word register, general purpose" R0)
  (defidform #:kind "word register, general purpose" R1)
  (defidform #:kind "word register, general purpose" R2)
  (defidform #:kind "word register, general purpose" R3)
  (defidform #:kind "word register, general purpose" R4)
  (defidform #:kind "word register, general purpose" R5)
  (defidform #:kind "word register, general purpose" R6)
  (defidform #:kind "word register, general purpose" R7)
  (defidform #:kind "address register, stack pointer" SP)
  (defidform #:kind "address register, program counter" PC)
  (defidform #:kind "instruction register" IR))]}

When called with an integer, it stores the integer in its input without changing its output.
The integer is truncated to its 64/20 lower significant bits in case of a word/address.
When called with @nbr['clock], it copies its input to its output.
In all cases the output is returned.
Register @tt{@bold{Rx}} is printed as @tt{#<Rx:h...>} where each @tt{h} is an hexadecimal digit,
16 digits for a word and 5 digits for an address.

@defproc[#:kind "predicate" (R? (‹obj› any/c)) boolean?]{
 True only if @nbr[‹obj›] is a @elemref["Rx"]{register} @tt{@bold{R0}} :: @tt{@bold{R7}},
 @tt{@bold{SP}}, @tt{@bold{PC}} or @tt{@bold{IR}}.}

@defproc[#:kind "predicate" (W? (‹obj› any/c)) boolean?]{
 True only if @nbr[‹obj›] is a @elemref["Rx"]{word register} @tt{@bold{R0}} :: @tt{@bold{R7}},
 or @tt{@bold{IR}}.}

@defproc[#:kind "predicate" (A? (‹obj› any/c)) boolean?]{
 True only if @nbr[‹obj›] is an @elemref["Rx"]{address register}:
 @tt{@bold{SP}} or @tt{@bold{PC}}.}

@defform[(clock (‹R› ‹value›) ...)
         #:contracts ((‹R› register?) (‹value› exact-integer?))]{
 Same as @nbr[(begin (‹R› ‹value›) ... (‹R› 'clock) ...)]}

@Interaction*[
 (void (clock (R0 #xA) (PC -10)))
 (list R0 PC)]

In the following example @tt{@bold{R1}} receives the output of @tt{@bold{R0}}
as present before clocking.

@Interaction*[
 (void (clock (R0 #xB) (R1 (R0))))
 (list R0 R1)]
@(reset-Interaction*)

@defproc[(~wx (‹e› real?)) string?]{
 Same as @nbr[(string-upcase (~r ‹e› #:min-width 16 #:base 16 #:pad-string "0"))].@(lb)
 For printing words.

 @Interaction[
 (clock (R0 -10))
 (printf "~a~n" (~wx (R0)))]}
         
@defproc[(~ax (‹e› real?)) string?]{
 Same as @nbr[(string-upcase (~r ‹e› #:min-width 5 #:base 16 #:pad-string "0"))].@(lb)
 For printing addresses.}

@section{Examples}
@subsection[#:tag "sec-factorial"]{Factorial}

@Interaction[
 (assemble
   '((: the R0-th factorial will be computed)
     (INP R0)
     (: the factorial is computed in R1)
     (SET R1 1)
     (: R2 is constant 1)
     (SET R2 1)
     (: do the computation)
     (LE? R0 R2 end)
     (loop : MUL R1 R0 R1)
     (SUB R0 R0 R2)
     (GT? R0 R2 loop)
     (end : OUT R1)))
 (parameterize ((INP-port (open-input-string "5")))
   (execute))]

@subsection{Fibonacci}

R0 : The number of elements to be computed and printed, possibly a few more.@(lb)
R1 : Fibonacci number, initially 0@(lb)
R2 : Next fibonacci number, initially 1@(lb)
R4 : 1 for decrementing R0

@Interaction[
 (parameterize ((show #f))
   (assemble
     '((SET R0 10)
       (SET R1 0)
       (SET R2 1)
       (SET R4 1)
       (=0? R0 end)
       (loop : OUT R1)
       (ADD R3 R1 R2)
       (SET R1 R2)
       (SET R2 R3)
       (SUB R0 R0 R4)
       (>0? R0 loop)
       (end : OUT R1)
       (OUT R2)
       (STP))))
 (let ((op (open-output-string)))
   (parameterize ((OUT-port op) (show '(instructions)))
     (execute)
     (display (get-output-string op))))]

@subsection{Subroutine call}

A subroutine j → 2j+1.
j is read from the @nbr[INP-port],@(lb)
the subroutine is called and the result printed.

@Interaction[
 (parameterize ((INP-port (open-input-string "3")) (show '(instructions)))
   (execute
     '((: Read j into R0)
       (INP R0)
       (: call the subroutine)
       (PSH R0)
       (PSH return)
       (JMP subroutine)
       (: Upon return fetch the result and print it)
       (return : POP R0)
       (OUT R0)
       (STP)
       (subroutine : POP R5) (: fetch return address)
       (POP R6) (: fetch j)
       (SET R1 1)
       (SET R2 2)
       (MUL R6 R6 R2)
       (ADD R6 R6 R1)
       (: fetch 2j+1 from R6 and return it via the stack)
       (PSH R6) (JMP R5))))
 (print-stack 5)]

@subsection{Self-modification}

By means of instruction @tt{MWR} a program can modify itself.
In the following example instruction @green{@tt{NOP}} at address @green{@tt{noot}}
is replaced by instruction @green{@tt{OUT}} found at address @green{@tt{aap}}.

@Interaction[
 (parameterize ((show '(instructions registers)))
   (execute
     '((MRD R1 aap)
       (MWR R1 noot)
       (NOP)
       (noot : NOP)
       (STP)
       (aap : OUT R1))))]

@subsection{Loading binary code}

Same as in section @nbsl["sec-factorial"]{factorial}
but the code is put in an input-string and read from this string.

@Interaction[
 (code:comment "Assemble factorial code")
 (define factorial
   (assemble
     '((SET R0 5)
       (SET R1 1)
       (SET R2 1)
       (loop : =0? R0 end)
       (MUL R1 R0 R1)
       (SUB R0 R0 R2)
       (JMP loop)
       (end : OUT R1))
     #:offset 5 #:target 'list))
 (code:comment "Put the code in a string-port")
 (define input
   (let ((op (open-output-string)))
     (for ((instr (in-list factorial))) (writeln (cdr instr) op))
     (open-input-string (get-output-string op))))
 (code:comment "Program reading from the string-port")
 (parameterize ((show #f))
   (assemble
     '((SET R0 5)
       (RÆD R0 8)
       (JMP R0))))
 (code:comment "Notice that the code for the factorial is not yet in memory")
 (print-memory 0 10)
 (parameterize ((show '(instructions)) (INP-port input)) (execute))]

@subsection{Integer square root}

@Interaction[
 (require (only-in racket open-output-nowhere))
 (parameterize
   ((current-output-port (open-output-nowhere)))
   (assemble
     '((INP R0)
       (SET R1 R0)
       (loop : MUL R2 R1 R1)
       (LE? R2 R0 end)
       (ADD R4 R2 R0)
       (MUL R5 R1 2)
       (DIV R1 R4 R5)
       (JMP loop)
       (end : OUT R0) (OUT R1)))
   (let ((p (open-output-string)))
     (parameterize ((OUT-port p))
       (for ((k (in-range 101)))
         (parameterize
           ((INP-port (open-input-string (format "~s" k)))
            (OUT-port p)
            (show #f))
           (execute))))
     (parameterize
       ((current-input-port (open-input-string (get-output-string p))))
       (let loop ((k 0))
         (cond
           ((equal? (read) eof) k)
           (else
             (let ((sqr (begin (read) (read) (read) (read)))
                   (root (begin (read) (read) (read) (read) (read))))
               (unless (= root (integer-sqrt sqr)) (error "root" sqr root)))
             (loop (add1 k))))))))]

@larger{@bold{The end}}
