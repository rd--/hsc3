# Haskell SuperCollider, a Tutorial.

## Prerequisites

Haskell SuperCollider requires that [SuperCollider][1], [GHC][2],
[Emacs][4] and the standard Haskell Emacs [mode][5] are all
installed and working properly.

## Setting up Haskell SuperCollider

Haskell SuperCollider is available through the haskell community
library system [Hackage][6].  To install type:

    $ cabal install hsc3

Haskell SuperCollider is also available as a set of [darcs][7]
repositories, the first implementing the [OpenSoundControl][9]
protocol, the second the SuperCollider bindings.

    $ darcs get http://slavepianos.org/rd/sw/hosc
    $ darcs get http://slavepianos.org/rd/sw/hsc3

To build use the standard Cabal process in each repository in
sequence.  To install to the user package database type:

    $ cabal install

## Setting up the Haskell SuperCollider Emacs mode

Add an appropriately modified variant of the following to
`~/.emacs`

    (push "~/sw/hsc3/emacs" load-path)
    (setq hsc3-help-directory "~/sw/hsc3/Help/")
    (require 'hsc3)

The hsc3 emacs mode associates itself with files having the
extension `.lhs`.  When the hsc3 emacs mode is active there is a
`Haskell SuperCollider` menu available.

## Literate Haskell

The documentation for Haskell SuperCollider, including this
tutorial, is written in _Bird_ notation, a form of literate
Haskell where lines starting with `>` are Haskell code and
everything else is commentary.

Unlike ordinary literate programs the Haskell SuperCollider help
files cannot be compiled to executables.  Each help file contains
multiple independant examples that can be evaluated using editor
commands, either by selecting from the `Haskell SuperCollider`
menu or using the associated keybinding.

## Interpreter Interaction & User Configuration

To start ghci type either `C-cC-b` or `C-cC-z` (Haskell -> Start
interpreter) to run `switch-to-haskell`.

Starting the interpreter splits the current window into two
windows.  If the ghci output window becomes obscured during a
session you can see it again by re-typing the commands above.

To interrupt ghci type `C-cC-i` (Haskell SuperCollider -> Haskell
-> Interrupt haskell).

To stop ghci type `C-cC-x` (Haskell SuperCollider -> Haskell ->
Quit haskell).

## Starting the SuperCollider server

The SuperCollider server can be started from the command line.
The help files assume that scsynth is listening for UDP
connections at the standard port on the local machine.

    $ scsynth -u 57110

## Basic SuperCollider Interaction

The SuperCollider server manages a graph of nodes with integer
identifiers.  The root node has ID zero.  By convention ordinary
graph nodes are placed in a group with identifier 1, however this
node is not created when scsynth starts.

To create this node we need to send an OSC message to the server,
the expression to do this is written below.  To run single line
expressions move the cursor to the line and type `C-cC-c` (Haskell
SuperCollider -> Expression -> Run line).

> import Sound.SC3

> withSC3 (\fd -> send fd (g_new [(1, AddToTail, 0)]))

We can then audition a quiet sine oscillator at A440.

> audition (out 0 (sinOsc AR 440 0 * 0.1))

To stop the sound we can delete the group it is a part of, the
audition function places the synthesis node into the group node
with ID 1, the expression below deletes that group.

> withSC3 (\fd -> send fd (n_free [1]))

In order to audition another graph we need to re-create a group
with ID 1.  `Sound.SC3` includes a function `reset` that sequences
these two actions, first deleting the group node, then
re-creating a new empty group.

> withSC3 reset

Using this command is so common there is a keybinding for it,
C-cC-k (Haskell SuperCollider -> SCSynth -> Reset scsynth).
After a reset we can audition a new graph.

> audition (out 0 (sinOsc AR 220 0 * 0.1))

Likewise auditioning a value is so common there is a keybinding,
C-cC-a.  Evaluating the line below names a UGen value, by placing
point over the name and typing C-cC-a the value is auditioned.

> let o = sinOsc AR 440 0 * 0.1

There is a variant that auditions an IO UGen value which has the
keybinding C-cC-z.

> import Sound.SC3.Monadic

> let n :: IO UGen ; n = whiteNoise AR >>= return . (* 0.1)

To see the server status type `C-cC-p` (Haskell SuperCollider ->
SCSynth -> Display status).  This prints a table indicating
server activity to the ghci output window.

    ***** SuperCollider Server Status *****
    # UGens                     Int 3
    # Synths                    Int 1
    # Groups                    Int 2
    # Instruments               Int 1
    % CPU (Average)             Float 2.6957032680511475
    % CPU (Peak)                Float 2.7786526679992676
    Sample Rate (Nominal)       Double 44100.0
    Sample Rate (Actual)        Double 44099.958404246536

## Completion messages

To send a completion message add one to an existing
asynchronous message using withCM.

> let { g = out 0 (sinOsc AR 660 0 * 0.15)
>     ; m = d_recv (synthdef "sin" g)
>     ; cm = s_new "sin" 100 AddToTail 1 [] }
> in withSC3 (\fd -> send fd (withCM m cm))

Alternately use variant constructors for the
asynchronous commands.

> import Sound.SC3.Server.Command.Completion

> let { g = out 0 (sinOsc AR 660 0 * 0.15)
>     ; cm = s_new "sin" 100 AddToTail 1 []
>     ; m = d_recv' cm (synthdef "sin" g) }
> in withSC3 (\fd -> send fd m)

## Controls

In hsc3 control parameters must be indexed by name.

There are four types of control parameters,
initialisation-rate (IR), control-rate (KR),
triggered-control-rate (TR) and audio-rate (AR).

The graph below illustrates the first three of these.
Note the specialised constructor for triggered
controls.

> let { b1 = control IR "b1" 0
>     ; b2 = control IR "b2" 1
>     ; f1 = control KR "f1" 450
>     ; f2 = control KR "f2" 900
>     ; a1 = tr_control "a1" 0
>     ; a2 = tr_control "a2" 0
>     ; m = impulse KR 1 0 * 0.1
>     ; d x = decay2 (m + x) 0.01 0.2
>     ; o1 = sinOsc AR f1 0 * d a1
>     ; o2 = saw AR f2 * d a2
>     ; g = mrg2 (out b1 o1) (out b2 o2)
>     ; i fd = do { _ <- async fd (d_recv (synthdef "g" g))
>                 ; send fd (s_new "g" 100 AddToTail 1 []) } }
> in withSC3 i

The output buses cannot be set, since they are
initialisation rate only.

> withSC3 (\fd -> send fd (n_set1 100 "b1" 1))
> withSC3 (\fd -> send fd (n_set1 100 "b2" 0))

The frequency controls can be set since they are
control rate.

> withSC3 (\fd -> send fd (n_set1 100 "f1" 200))
> withSC3 (\fd -> send fd (n_set1 100 "f2" 300))

The audio controls can be set, however they are
immediately reset to zero at the next control cycle.

> withSC3 (\fd -> send fd (n_set1 100 "a1" 1))
> withSC3 (\fd -> send fd (n_set1 100 "a2" 1))

## Multiple line expressions

There are two variants for expressions that are written over
multiple lines.

To evaluate an expression that is written without using the
Haskell layout rules select the region and type `C-cC-e` (Haskell
SuperCollider -> Expression -> Run multiple lines).  To select a
region use the mouse or place the cursor at one end, type
`C-[Space]` then move the cursor to the other end.

> let { f0 = xLine KR 1 1000 9 RemoveSynth
>     ; f1 = sinOsc AR f0 0 * 200 + 800 }
> in audition (out 0 (sinOsc AR f1 0 * 0.1))

To evaluate a multiple line expression written using the layout
rules as applicable within a do block, select the region and type
`C-cC-r` (Haskell SuperCollider -> Expression -> Run region).

> let f0 = xLine KR 1 1000 9 RemoveSynth
>     f1 = sinOsc AR f0 0 * 200 + 800
> audition (out 0 (sinOsc AR f1 0 * 0.1))

This writes the region in a do block in a procedure to a
temporary file, /tmp/hsc3.lhs, loads the file and then runs the
procedure.  The preamble imports the modules listed at the emacs
variable hsc3-modules.

ghci understands import expressions, so to add a module to the
current scope it is enough to type `C-cC-c` at an appropriate
location.  If hsc3-dot is installed, the following two
expressions will load the module and make a drawing.

> import Sound.SC3
> import Sound.SC3.UGen.Dot

> let { o = control KR "bus" 0
>     ; f = mouseX KR 440 880 Exponential 0.1 }
> in draw (out o (sinOsc AR f 0))

## Help Files

To find help on a unit generator or on a SuperCollider server
command place the cursor over the identifier and type `C-cC-h`
(Haskell SuperCollider -> Help -> Haskell SuperCollider help).
This opens the help file, which ought to have working examples in
it, the above graph is in the sinOsc help file, the s_new help
file explains what arguments are required and what they mean.

The Haskell SuperCollider help files are derived from the help
files distributed with SuperCollider, the text is re-formatted to
read well as plain text and examples are translated into Haskell.

There is also partial haddock documentation for the Sound.SC3 and
Sound.OpenSoundControl modules, to build type:

    $ runhaskell Setup.lhs haddock

## Identifier lookup & hasktags

The emacs command M-. (find-tag) looks up an identifier in a
_tags_ table.  The hasktags utility can generate tags files from
haskell source files that are usable with emacs.

To generate a tags file for hsc3, visit the hsc3 directory and
type:

    $ find Sound -name '*.*hs' | xargs hasktags -e

To use the hsc3 tags table type `M-x visit-tags-table`, or add an
entry to ~/.emacs:

    (setq tags-table-list '("~/sw/hsc3"))

## External Unit Generators

hsc3 includes bindings and help files for some unit generators not in
the standard supercollider distribution.  In order to use these unit
generators they must be installed, see [sc3-plugins][8].


## Example Unit Generator Graphs

The `Help` directory contains example unit generator
graphs.  To audition a graph from Emacs type `C-cC-l`
`C-cC-m`.  Many of the graphs are self contained,
selecting the graph (excluding the 'main =' line) and
typing `C-cC-e` will audition it.  In many cases both
supercollider language and haskell versions are given,
switch the emacs buffer to sclang-mode to run the
supercollider language versions.

## Monitoring incoming server messages

To monitor what OSC messages scsynth is receiving use the
`dumpOSC` server command to request that scsynth print text
traces of incoming messages to its standard output.

> withSC3 ((flip send) (dumpOSC TextPrinter))

To end printing send:

> withSC3 ((flip send) (dumpOSC NoPrinter))

## UGen structure

> import Sound.SC3

There is a right fold.

> ugenFoldr (\u r -> ugenType u : r) [] (sinOsc AR 440 0 * 0.1)
> ugenFoldr (\u r -> show u : r) [] (sinOsc AR 440 0 * 0.1)

## References

[1]: http://audiosynth.com/
[2]: http://haskell.org/ghc/
[4]: http://gnu.org/software/emacs/
[5]: http://haskell.org/haskell-mode/
[6]: http://hackage.haskell.org/
[7]: http://darcs.net/
[8]: http://sf.net/projects/sc3-plugins
[9]: http://opensoundcontrol.org/