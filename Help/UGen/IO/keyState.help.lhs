> Sound.SC3.UGen.Help.viewSC3Help "KeyState"
> Sound.SC3.UGen.DB.ugenSummary "KeyState"

> import Sound.SC3

The keycode 38 is the A key on my keyboard.  Under X the xev(1)
command is useful in determining your keyboard layout.
> audition (out 0 (sinOsc AR 800 0 * keyState KR 38 0 0.1 0.5))
