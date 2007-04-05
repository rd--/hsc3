mouseX rate minval maxval warp lag

Cursor UGen

Report mouse location on root window of the machine that the synthesis
server is running on.

> audition (out 0 (sinOsc AR (mouseX KR 40 10000 Exponential 0.2) 0 * 0.1))
