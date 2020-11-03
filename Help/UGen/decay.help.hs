-- decay ; as envelope
let n = pinkNoise 'α' AR + sinOsc AR 11000 0
    s = impulse AR (xLine KR 1 50 20 RemoveSynth) 0.25
in decay s 0.05 * n

---- ; drawings
Sound.SC3.Plot.plot_ugen1 0.05 (decay (impulse AR 1 0) 0.01)
