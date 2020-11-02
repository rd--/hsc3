-- lfdNoise3 ; c.f. lfdNoise0
lfdNoise3 'α' AR (xLine AR 1000 100 0.1 DoNothing)

---- ; drawings
Sound.SC3.Plot.plot_ugen1 0.1 (lfdNoise3 'α' AR (xLine AR 1000 100 0.1 DoNothing))
