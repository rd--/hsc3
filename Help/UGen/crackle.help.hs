-- crackle
crackle AR 1.95 * 0.2

-- crackle ; modulate chaos parameter
crackle AR (line KR 1.0 2.0 3 RemoveSynth) * 0.2

---- ; drawings
Sound.SC3.Plot.plot_ugen1 0.01 (crackle AR 1.95)
Sound.SC3.Plot.plot_ugen1 0.025 (crackle AR (line KR 1.0 2.0 0.025 DoNothing))
