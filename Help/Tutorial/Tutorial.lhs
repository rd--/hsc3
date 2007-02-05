* Haskell SuperCollider, a Tutorial.

* Prerequisites

Hsc requires that SuperCollider [1], GHC [2], Emacs [3] and the
standard Haskell Emacs mode [4] are all installed and working
properly.

* Setting up Hsc

Hsc is currently only available as a darcs repository.

  $ darcs get http://slavepianos.org/rd/sw/sw-69

To build Hsc use the standard Cabal process:

  $ runhaskell Setup.hs configure --prefix ~
  $ runhaskell Setup.hs build
  $ runhaskell Setup.hs install --user

* Setting up the Hsc emacs mode

Add an appropriately modified variant of the following to ~/.emacs

  (push "~/sw/sw-69/emacs" load-path)
  (setq hsc-interpreter "ghci")
  (setq hsc-help-directory "~/sw/sw-69/Help/")
  (require 'hsc)

The Hsc mode associates itself with files having the extension '.lhs'.
When the Hsc emacs mode is active there is an Hsc menu available.

* Literate Haskell

The documentation for Hsc, including this tutorial, is written in
'Bird' notation, a form of 'literate Haskell' where lines starting
with '>' are Haskell code and everything else is commentary.

Unlike ordinary literate programs the Hsc help files cannot be
compiled to executables.  Each help file contains multiple independant
examples that can be evaluated using editor commands, either by
selecting from the Hsc menu or using the associated keybinding.

* Interpreter Interaction

To start ghci and load the list of modules at the emacs variable
'hsc-modules' use C-cC-s (Hsc -> Haskell -> Start haskell).  By
default 'hsc-modules' contains Sound.OpenSoundControl, Sound.SC3,
Control.Monad and System.Random.

This splits the current window into two windows.  If the ghci output
window becomes obscured during a session you can see it again by
typing C-cC-g (Hsc -> Haskell -> See output).

To stop ghci type C-cC-x (Hsc -> Haskell -> Quit haskell).

* Starting the SuperCollider server

The SuperCollider server can be started from the command line.

  scsynth -u 57110

* Basic SuperCollider Interaction

The SuperCollider server manages a graph of nodes with integer
identifiers.  The root node has ID zero.  By convention ordinary graph
nodes are placed in a group with identifier 1, however this node is
not created when scsynth starts.

To create this node we need to send an OSC message to the server, the
expression to do this is written below.  To run single line
expressions move the cursor to the line and type C-cC-c (Hsc ->
Expression -> Run line).

> withSC3 (\fd -> send fd (g_new [(1, AddToTail, 0)]))

We can then audition a quiet sine oscillator at A440.

> audition (sinOsc AR 440 0 * 0.1)

To stop the sound we can delete the group it is a part of, the
audition function places the synthesis node into the group node with
ID 1, the expression below deletes that group.

> withSC3 (\fd -> send fd (n_free [1]))

In order to audition another graph we need to re-create a group with
ID 1.  Sound.SC3 includes a function 'reset' that sequences these two
actions, first deleting the group node, then re-creating a new empty
group.

> withSC3 reset

Using this command is so common there is a keybinding for it, C-cC-k
(Hsc -> SCSynth -> Reset scsynth).  After a reset we can audition a
new graph.

> audition (sinOsc AR 220 0 * 0.1)

To see the server status type C-cC-w (Hsc -> SCSynth -> Display
status).  This prints a table indicating server activity to the ghci
output window.

  ***** SuperCollider Server Status *****
  # UGens                     3
  # Synths                    1
  # Groups                    2
  # Instruments               1
  % CPU (Average)             2.6957032680511475
  % CPU (Peak)                2.7786526679992676
  Sample Rate (Nominal)       44100.0
  Sample Rate (Actual)        44099.958404246536

* Multiple line expressions

To evaluate expressions that don't fit on one line select the region
and type C-cC-e (Hsc -> Expression -> Run region).  To select a region
use the mouse or place the cursor at one end, type C-[Space] then move
the cursor to the other end.

> let f = sinOsc AR (xLine KR 1 1000 9 RemoveSynth) 0 * 200 + 800
> audition (sinOsc AR f 0 * 0.1)

This writes the region in a do block in a procedure to a temporary
file, /tmp/hsc.lhs, loads the file and then runs the procedure.  The
preamble imports the modules listed at the emacs variable hsc-modules.

* Help Files

To find help on a UGen or on a SuperCollider server command place the
cursor over the identifier and type C-cC-h (Hsc -> Help -> Hsc help).
This opens the help file, which ought to have working examples in it,
the above graph is in the sinOsc help file, the s_new help file
explains what arguments are required and what they mean.

The Hsc help files are derived from the help files distributed with
SuperCollider, the text is re-formatted to read well as plain text and
examples are translated into Haskell.

There is also partial haddock documentation for the Sound.SC3 and
Sound.OpenSoundControl modules, to build type:

  $ runhaskell Setup.hs haddock

* References

[1] http://www.audiosynth.com/
[2] http://www.haskell.org/ghc/
[3] http://www.gnu.org/software/emacs/
[4] http://www.haskell.org/haskell-mode/
