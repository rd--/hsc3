-- crackle
crackle ar 1.95 * 0.2

-- crackle ; modulate chaos parameter
crackle ar (line kr 1.0 2.0 3 RemoveSynth) * 0.2

---- ; drawings
Sound.Sc3.Plot.plot_ugen1 0.01 (crackle ar 1.95)
Sound.Sc3.Plot.plot_ugen1 0.025 (crackle ar (line kr 1.0 2.0 0.025 DoNothing))
