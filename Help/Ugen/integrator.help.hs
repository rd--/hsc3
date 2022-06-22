-- integrator
let x = mouseX kr 0.001 0.999 Exponential 0.2
    o = lfPulse ar 300 0.2 0.1 * 0.1
in integrator o x

-- integrator ; used as an envelope
let i = lfPulse ar 3 0.2 0.0004
    o = sinOsc ar 700 0 * 0.1
in integrator i 0.999 * o

-- integrator ; scope
let x = mouseX kr 0.01 0.999 Exponential 0.2
    o = lfPulse ar (1500 / 4) 0.2 0.1
in integrator o x * 0.1

-- integrator ; a triangle wave is the integration of square wave
let f = mouseX kr 440 8800 Exponential 0.2
    o = pulse ar f 0.5
in integrator o 0.99 * 0.05

---- ; drawings
UI.ui_sc3_scope 2 0 (2 ^ 14) 0 "audio" 0
Sound.Sc3.Plot.plot_ugen 0.006 (integrator (lfPulse ar (1500 / 4) 0.2 0.1) (mce [0.1,0.4,0.7]))
