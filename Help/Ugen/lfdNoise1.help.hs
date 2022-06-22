-- lfdNoise1 ; c.f. lfdNoise0 & lfdNoise3
lfdNoise1Id 'α' ar (xLine ar 1000 100 0.1 DoNothing)

---- ; drawings ; c.f. lfdNoise0 & lfdNoise3
Sound.Sc3.Plot.plot_ugen1 0.1 (lfdNoise1Id 'α' ar (xLine ar 1000 100 0.1 DoNothing))
