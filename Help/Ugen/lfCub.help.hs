-- lfCub ; c.f. lfPar
let g f = f ar (f kr (f kr 0.2 0 * 8 + 10) 0 * 400 + 800) 0 * 0.1 in mce2 (g lfCub) (g lfPar)

-- lfCub ; c.f. lfPar
let g f = f ar (f kr 0.2 0 * 400 + 800) 0 * 0.1 in mce2 (g lfCub) (g lfPar)

-- lfCub ; c.f. lfPar
mce2 (lfCub ar 800 0 * 0.1) (lfPar ar 800 0 * 0.1)

-- lfCub ; c.f. lfPar
let g f = f ar (xLine kr 100 8000 30 DoNothing) 0 * 0.1 in mce2 (g lfCub) (g lfPar)

-- lfCub ; c.f. sinOsc
let g f = f ar (f kr (f kr 0.2 0 * 8 + 10) 0 * 400 + 800) 0 * 0.1 in mce2 (g lfCub) (g sinOsc)

-- lfCub ; c.f. sinOsc
let g f = f ar (f kr 0.2 0 * 400 + 800) 0 * 0.1 in mce2 (g lfCub) (g sinOsc)

-- lfCub ; c.f. sinOsc
mce2 (lfCub ar 800 0 * 0.1) (sinOsc ar 800 0 * 0.1)

-- lfCub ; c.f. sinOsc
let g f = f ar (xLine kr 100 8000 30 DoNothing) 0 * 0.1 in mce2 (g lfCub) (g sinOsc)

-- lfCub ; c.f. lfTri
let g f = f ar (f kr (f kr 0.2 0 * 8 + 10) 0 * 400 + 800) 0 * 0.1 in mce2 (g lfCub) (g lfTri)

-- lfCub ; c.f. lfTri
let g f = f ar (f kr 0.2 0 * 400 + 800) 0 * 0.1 in mce2 (g lfCub) (g lfTri)

-- lfCub ; c.f. lfTri
mce2 (lfCub ar 800 0 * 0.1) (lfTri ar 800 0 * 0.1)

-- lfCub ; c.f. lfTri
let g f = f ar (xLine kr 100 8000 30 DoNothing) 0 * 0.1 in mce2 (g lfCub) (g lfTri)

---- ; drawings
Sound.Sc3.Plot.plot_ugen 0.1 (mce2 (sinOsc ar 20 0) (lfCub ar 20 0))
