mouseButton rate minval maxval lag

Report the status of the first pointer button.  The button is either
pressed, or not pressed.

> import Sound.SC3

> audition (out 0 (sinOsc AR 800 0 * mouseButton KR 0 0.1 0.1))
