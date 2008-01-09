* Haskell SuperCollider, a Tutorial.

* Prerequisites

Haskell SuperCollider requires that SuperCollider [1], GHC [2], the
GHC binary package[3], Emacs [4] and the standard Haskell Emacs mode
[5] are all installed and working properly.

* Setting up Haskell SuperCollider

Haskell SuperCollider is currently only available as a set of darcs
repositories, the first implementing the Sound.OpenSoundControl
module, the second the Sound.SC3 module.

  $ darcs get http://slavepianos.org/rd/sw/hosc
  $ darcs get http://slavepianos.org/rd/sw/hsc3

To build use the standard Cabal process in each repository in
sequence.

  $ runhaskell Setup.lhs configure --prefix ~
  $ runhaskell Setup.lhs build
  $ runhaskell Setup.lhs install --user

* Setting up the Haskell SuperCollider Emacs mode

Add an appropriately modified variant of the following to ~/.emacs

  (push "~/sw/hsc3/emacs" load-path)
  (setq hsc3-interpreter "ghci")
  (setq hsc3-help-directory "~/sw/hsc3/Help/")
  (require 'hsc3)

The hsc3 emacs mode associates itself with files having the extension
'.lhs'.  When the hsc3 emacs mode is active there is a 'Haskell
SuperCollider' menu available.

* Literate Haskell

The documentation for Haskell SuperCollider, including this tutorial,
is written in 'Bird' notation, a form of 'literate Haskell' where
lines starting with '>' are Haskell code and everything else is
commentary.

Unlike ordinary literate programs the Haskell SuperCollider help files
cannot be compiled to executables.  Each help file contains multiple
independant examples that can be evaluated using editor commands,
either by selecting from the 'Haskell SuperCollider' menu or using the
associated keybinding.

* Interpreter Interaction

To start ghci and load the list of modules at the emacs variables
'hsc3-main-modules' and 'hsc3-modules' use C-cC-s (Haskell
SuperCollider -> Haskell -> Start haskell).  By default
'hsc3-main-modules' is empty and 'hsc3-modules' contains
Sound.OpenSoundControl, Sound.SC3, Data.List, Control.Monad,
Control.Concurrent, System.Directory and System.Random.

Starting the interpreter splits the current window into two windows.  If
the ghci output window becomes obscured during a session you can see
it again by typing C-cC-g (Haskell SuperCollider -> Haskell -> See
output).

To stop ghci type C-cC-x (Haskell SuperCollider -> Haskell -> Quit
haskell).

* Starting the SuperCollider server

The SuperCollider server can be started from the command line.  The
help files assume that scsynth is listening for UDP connections at the
standard port on the local machine.

  $ scsynth -u 57110

* Basic SuperCollider Interaction

The SuperCollider server manages a graph of nodes with integer
identifiers.  The root node has ID zero.  By convention ordinary graph
nodes are placed in a group with identifier 1, however this node is
not created when scsynth starts.

To create this node we need to send an OSC message to the server, the
expression to do this is written below.  To run single line
expressions move the cursor to the line and type C-cC-c (Haskell
SuperCollider -> Expression -> Run line).

> withSC3 (\fd -> send fd (g_new [(1, AddToTail, 0)]))

We can then audition a quiet sine oscillator at A440.

> audition (out 0 (sinOsc AR 440 0 * 0.1))

To stop the sound we can delete the group it is a part of, the
audition function places the synthesis node into the group node with
ID 1, the expression below deletes that group.

> withSC3 ((flip send) (n_free [1]))

In order to audition another graph we need to re-create a group with
ID 1.  Sound.SC3 includes a function 'reset' that sequences these two
actions, first deleting the group node, then re-creating a new empty
group.

> withSC3 reset

Using this command is so common there is a keybinding for it, C-cC-k
(Haskell SuperCollider -> SCSynth -> Reset scsynth).  After a reset we
can audition a new graph.

> audition (out 0 (sinOsc AR 220 0 * 0.1))

To see the server status type C-cC-w (Haskell SuperCollider -> SCSynth
-> Display status).  This prints a table indicating server activity to
the ghci output window.

  ***** SuperCollider Server Status *****
  # UGens                     Int 3
  # Synths                    Int 1
  # Groups                    Int 2
  # Instruments               Int 1
  % CPU (Average)             Float 2.6957032680511475
  % CPU (Peak)                Float 2.7786526679992676
  Sample Rate (Nominal)       Double 44100.0
  Sample Rate (Actual)        Double 44099.958404246536

* Multiple line expressions

There are two variants for expressions that are written over multiple
lines.

To evaluate an expression that is written without using the Haskell
layout rules select the region and type C-cC-r (Haskell SuperCollider
-> Expression -> Run multiple lines).  To select a region use the
mouse or place the cursor at one end, type C-[Space] then move the
cursor to the other end.

> let { f0 = xLine KR 1 1000 9 RemoveSynth
>     ; f1 = sinOsc AR f0 0 * 200 + 800 }
> in audition (out 0 (sinOsc AR f1 0 * 0.1))

To evaluate a multiple line expression written using the layout rules
as applicable within a do block, select the region and type C-cC-e
(Haskell SuperCollider -> Expression -> Run region).

> let f0 = xLine KR 1 1000 9 RemoveSynth
>     f1 = sinOsc AR f0 0 * 200 + 800
> audition (out 0 (sinOsc AR f1 0 * 0.1))

This writes the region in a do block in a procedure to a temporary
file, /tmp/hsc3.lhs, loads the file and then runs the procedure.  The
preamble imports the modules listed at the emacs variable
hsc3-modules.

* Help Files

To find help on a UGen or on a SuperCollider server command place the
cursor over the identifier and type C-cC-h (Haskell SuperCollider ->
Help -> Haskell SuperCollider help).  This opens the help file, which
ought to have working examples in it, the above graph is in the sinOsc
help file, the s_new help file explains what arguments are required
and what they mean.

The Haskell SuperCollider help files are derived from the help files
distributed with SuperCollider, the text is re-formatted to read well
as plain text and examples are translated into Haskell.

There is also partial haddock documentation for the Sound.SC3 and
Sound.OpenSoundControl modules, to build type:

  $ runhaskell Setup.lhs haddock

* User configuration files

It is straightforward to add configuration modules to the context in
which the hsc3 emacs mode evaluates expressions using the
'hsc3-main-modules' variable.  The search path for these modules can
be set using 'hsc3-interpreter-arguments'.

 (setq hsc3-interpreter-arguments (list "-i/home/rohan/.hsc3"))
 (setq hsc3-main-modules (list "Config"))

* Monitoring incoming server messages

To monitor what OSC messages scsynth is receiving use the 'dumpOSC'
server command to request that scsynth print text traces of incoming
messages to its standard output.

> withSC3 ((flip send) (dumpOSC TextPrinter))

To end printing send:

> withSC3 ((flip send) (dumpOSC NoPrinter))

* References

[1] http://www.audiosynth.com/
[2] http://www.haskell.org/ghc/
[3] http://hackage.haskell.org/cgi-bin/hackage-scripts/package/binary-0.3
[4] http://www.gnu.org/software/emacs/
[5] http://www.haskell.org/haskell-mode/
