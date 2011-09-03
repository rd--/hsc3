> Sound.SC3.UGen.Help.viewSC3Help "MouseButton"
> Sound.SC3.UGen.DB.ugenSummary "MouseButton"

> import Sound.SC3

As amplitude envelope
> audition (out 0 (sinOsc AR 800 0 * mouseButton KR 0 0.1 0.1))

There is a variant that randomly presses the button.
> audition (out 0 (sinOsc AR 800 0 * mouseButton' KR 0 0.1 0.1))
