-- keyState ; keycode 38 is the A key on some keyboards ; under X11 see xev(1) to determine layout
sinOsc ar 800 0 * keyState kr 38 0 0.1 0.5
