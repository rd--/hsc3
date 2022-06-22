-- decay ; as envelope
let n = pinkNoiseId 'α' ar + sinOsc ar 11000 0
    s = impulse ar (xLine kr 1 50 20 RemoveSynth) 0.25
in decay s 0.05 * n

---- ; drawings
Sound.Sc3.Plot.plot_ugen1 0.05 (decay (impulse ar 1 0) 0.01)
