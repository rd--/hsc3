# Haskell SuperCollider, a Tutorial.

## Prerequisites

Haskell SuperCollider ([hsc3][13])requires that [SuperCollider][1],
[GHC][2], [Emacs][4] and [haskell-mode][5] are all installed and
working properly.

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

`hsc3-mode` is a derivative of `haskell-mode` and uses
`inferior-haskell-mode` for communicating with a `ghci` process.

The manuals for these packages document many useful commands
(ie. `C-cC-t` gets type information, `C-uC-cC-t` inserts an inferred
type signature and so on).

## Literate Haskell

The documentation for Haskell SuperCollider, including this tutorial,
is written using _Bird notation_, a form of literate Haskell where
lines starting with `>` are Haskell code and everything else is
commentary.

The commentary is written using the [pandoc][14] variant of
[markdown][15] so that the hsc3 documentation can be rendered in a
number of formats.

Unlike ordinary literate programs the Haskell SuperCollider
documentation and help files cannot be compiled to executables.  Each
file contains multiple independant examples that can be evaluated
using editor commands, either by selecting from the `Haskell` or
`Haskell-SuperCollider` menus or using the associated keybinding.

## Interpreter Interaction & User Configuration

To see the haskell inferior process buffer, and to start the
interpreter if it is not running, type either `C-c>` (Haskell
SuperCollider -> Haskell -> See haskell).

This splits the current window into two windows and is used if the
ghci output window becomes obscured during a session.  Unlike `C-cC-z`
(`switch-to-haskell`) it leaves point in the current buffer, sets the
prompt to `hsc3>` and shows the end of the process buffer.

To interrupt ghci type `C-cC-i` (Haskell SuperCollider -> Haskell ->
Interrupt haskell).  To interrupt ghci and then reset scsynth type
`C-cC-s` (ie. this is equal to typing `C-cC-i` and then `C-cC-k`).

To exit ghci type `C-cC-q` (Haskell SuperCollider -> Haskell -> Quit
haskell).

## Starting the SuperCollider server

The SuperCollider server can be started from the command line.
The help files assume that scsynth is listening for UDP
connections at the standard port on the local machine.

    $ scsynth -u 57110

The [hsc3-process][17] package by Stefan Kersten has bindings for
`libscsynth` and allows for direct control of the `scsynth` process
from within haskell.

## Basic SuperCollider Interaction

To send an expressions that is on a single line to the haskell
interpreter, move the cursor to the line and type `C-cC-c` (Haskell
SuperCollider -> Expression -> Run line).  `ghci` understands import
expressions, so to add a module to the current scope it is enough to
type `C-cC-c` at an appropriate location.

> import Sound.SC3

The SuperCollider server manages a graph of nodes with integer
identifiers.  The root node has identifier `0`.  By convention
ordinary graph nodes are placed in a group with identifier `1`,
however this node is not created when scsynth starts.

To create this node we need to send an OSC message to the server, the
expression to do this is written below.

> withSC3 (\fd -> send fd (g_new [(1, AddToTail, 0)]))

We can then audition a quiet sine oscillator at A440.

> audition (out 0 (sinOsc AR 440 0 * 0.1))

To stop the sound we can delete the group it is a part of, the
audition function places the synthesis node into the group node `1`,
the expression below deletes that group.

> withSC3 (\fd -> send fd (n_free [1]))

In order to audition another graph we need to re-create a group with
identifier `1`.  `Sound.SC3` includes a function `reset` that
sequences these two actions, first deleting the group node, then
re-creating a new empty group.

> withSC3 reset

Using this command is so common there is a keybinding for it,
`C-cC-k` (Haskell SuperCollider -> SCSynth -> Reset scsynth).
After a reset we can audition a new graph.

> audition (out 0 (sinOsc AR 220 0 * 0.1))

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

The [hsc3-server][16] package by Stefan Kersten has abstractions for
managing node, bus and buffer identifiers.

## Multiple line expressions

To evaluate an expression that is written over multiple lines select
the region and type `C-cC-e` (Haskell SuperCollider -> Expression ->
Run multiple lines).  To select a region use the mouse or place the
cursor at one end, type `C-[Space]` then move the cursor to the other
end.

> let {f0 = xLine KR 1 1000 9 RemoveSynth
>     ;f1 = sinOsc AR f0 0 * 200 + 800}
> in audition (out 0 (sinOsc AR f1 0 * 0.1))

Expressions spanning multiple lines are joined into one line before
being sent to the interpreter and therefore _must_ be written without
using the Haskell layout rules.

## UGen Graph Drawings

If [hsc3-dot][11] and [graphviz][18] are installed, the following two
expressions will load the required haskell module and make a drawing.

> import Sound.SC3.UGen.Dot

> let {o = control KR "bus" 0
>     ;f = mouseX KR 440 880 Exponential 0.1}
> in draw (out o (sinOsc AR f 0))

## Completion messages

To send a completion message add one to an existing
asynchronous message using withCM.

> let {g = out 0 (sinOsc AR 660 0 * 0.15)
>     ;m = d_recv (synthdef "sin" g)
>     ;cm = s_new "sin" 100 AddToTail 1 []}
> in withSC3 (\fd -> send fd (withCM m cm))

Alternately use variant constructors for the
asynchronous commands.

> import Sound.SC3.Server.Command.Completion

> let {g = out 0 (sinOsc AR 660 0 * 0.15)
>     ;cm = s_new "sin" 100 AddToTail 1 []
>     ;m = d_recv' cm (synthdef "sin" g)}
> in withSC3 (\fd -> send fd m)

## Controls

In hsc3 control parameters must be indexed by name.

There are four types of control parameters,
initialisation-rate (`IR`), control-rate (`KR`),
triggered-control-rate (`TR`) and audio-rate (`AR`).

The graph below illustrates the first three of these.
Note the specialised constructor for triggered
controls.

> let {b1 = control IR "b1" 0
>     ;b2 = control IR "b2" 1
>     ;f1 = control KR "f1" 450
>     ;f2 = control KR "f2" 900
>     ;a1 = tr_control "a1" 0
>     ;a2 = tr_control "a2" 0
>     ;m = impulse KR 1 0 * 0.1
>     ;d x = decay2 (m + x) 0.01 0.2
>     ;o1 = sinOsc AR f1 0 * d a1
>     ;o2 = saw AR f2 * d a2
>     ;g = mrg2 (out b1 o1) (out b2 o2)
>     ;act fd = do {_ <- async fd (d_recv (synthdef "g" g))
>                  ;send fd (s_new "g" 100 AddToTail 1 [])}}
> in withSC3 act

The output buses cannot be set, since they are
initialisation rate only.

> withSC3 (\fd -> send fd (n_set1 100 "b1" 1))
> withSC3 (\fd -> send fd (n_set1 100 "b2" 0))

The frequency controls can be set since they are
control rate.

> withSC3 (\fd -> send fd (n_set1 100 "f1" 200))
> withSC3 (\fd -> send fd (n_set1 100 "f2" 300))

The trigger controls can be set, however they are immediately reset to
zero at the next control cycle.

> withSC3 (\fd -> send fd (n_set1 100 "a1" 1))
> withSC3 (\fd -> send fd (n_set1 100 "a2" 1))

## Help Files

To find help on a unit generator or on a SuperCollider server
command place the cursor over the identifier and type `C-cC-h`
(Haskell SuperCollider -> Help -> Haskell SuperCollider help).
This opens the help file, which ought to have working examples in
it, the above graph is in the `sinOsc` help file, the `s_new` help
file explains what arguments are required and what they mean.

To open the SuperCollider help page for a UGen type `C-cC-j`.  This
requires that `SCDoc.renderAll` has been run at `sclang`.  If the SC3
documentation is not in the standard location set the `SC3_HELP`
environment variable.

To view a summary of a UGen, with input names and default values, type
`C-cC-u` (this requires that [hsc3-db][12] is installed).

There is also partial haddock documentation for the hosc and hsc3,
which is normally built by `cabal install`.

## Identifier lookup & hasktags

The emacs command `M-.` (find-tag) looks up an identifier in a
_tags_ table.  The hasktags utility can generate tags files from
haskell source files that are usable with emacs.

To generate a tags file for hsc3, visit the hsc3 directory and
type:

    $ find Sound -name '*.*hs' | xargs hasktags -e

This command can be run from within emacs using
`hsc3-update-hsc3-tags`.  To use the hsc3 tags table type `M-x
visit-tags-table`, or add an entry to `~/.emacs`:

    (setq tags-table-list '("~/sw/hsc3"))

## External Unit Generators

hsc3 includes bindings and help files for some unit generators not in
the standard supercollider distribution.  In order to use these unit
generators they must be installed, see [sc3-plugins][8].

## Example Unit Generator Graphs

The [hsc3-graphs][10] package contains example unit generator graphs.

These graphs all provide a `main` function, to load the file type
`C-cC-l` and to run the `main` function type `C-cC-m`.

In many cases parallel supercollider language versions are given in a
file with `.scd` extension.

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
[5]: http://code.haskell.org/haskellmode-emacs/
[6]: http://hackage.haskell.org/
[7]: http://darcs.net/
[8]: http://sf.net/projects/sc3-plugins
[9]: http://opensoundcontrol.org/
[10]: http://slavepianos.org/rd/?t=hsc3-graphs
[11]: http://slavepianos.org/rd/?t=hsc3-dot
[12]: http://slavepianos.org/rd/?t=hsc3-db
[13]: http://slavepianos.org/rd/?t=hsc3
[14]: http://johnmacfarlane.net/pandoc/
[15]: http://daringfireball.net/projects/markdown/
[16]: http://space.k-hornz.de/software/hsc3-server/
[17]: http://space.k-hornz.de/software/hsc3-process/
[18]: http://graphviz.org/
