keyState rate keyNum minVal maxVal lag

Report the status of a particular key.  A key is either pressed, or
not pressed.

The keycode 38 is the A key on my keyboard.  Under X the xev(1)
command is useful in determining your keyboard layout.

> audition (out 0 (sinOsc AR 800 0 * keyState KR 38 0 0.1 0.5))
