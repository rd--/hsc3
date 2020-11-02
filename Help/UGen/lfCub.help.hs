-- lfCub ; c.f. lfPar
let g f = f AR (f KR (f KR 0.2 0 * 8 + 10) 0 * 400 + 800) 0 * 0.1 in mce2 (g lfCub) (g lfPar)

-- lfCub ; c.f. lfPar
let g f = f AR (f KR 0.2 0 * 400 + 800) 0 * 0.1 in mce2 (g lfCub) (g lfPar)

-- lfCub ; c.f. lfPar
mce2 (lfCub AR 800 0 * 0.1) (lfPar AR 800 0 * 0.1)

-- lfCub ; c.f. lfPar
let g f = f AR (xLine KR 100 8000 30 DoNothing) 0 * 0.1 in mce2 (g lfCub) (g lfPar)

-- lfCub ; c.f. sinOsc
let g f = f AR (f KR (f KR 0.2 0 * 8 + 10) 0 * 400 + 800) 0 * 0.1 in mce2 (g lfCub) (g sinOsc)

-- lfCub ; c.f. sinOsc
let g f = f AR (f KR 0.2 0 * 400 + 800) 0 * 0.1 in mce2 (g lfCub) (g sinOsc)

-- lfCub ; c.f. sinOsc
mce2 (lfCub AR 800 0 * 0.1) (sinOsc AR 800 0 * 0.1)

-- lfCub ; c.f. sinOsc
let g f = f AR (xLine KR 100 8000 30 DoNothing) 0 * 0.1 in mce2 (g lfCub) (g sinOsc)

-- lfCub ; c.f. lfTri
let g f = f AR (f KR (f KR 0.2 0 * 8 + 10) 0 * 400 + 800) 0 * 0.1 in mce2 (g lfCub) (g lfTri)

-- lfCub ; c.f. lfTri
let g f = f AR (f KR 0.2 0 * 400 + 800) 0 * 0.1 in mce2 (g lfCub) (g lfTri)

-- lfCub ; c.f. lfTri
mce2 (lfCub AR 800 0 * 0.1) (lfTri AR 800 0 * 0.1)

-- lfCub ; c.f. lfTri
let g f = f AR (xLine KR 100 8000 30 DoNothing) 0 * 0.1 in mce2 (g lfCub) (g lfTri)

---- ; drawings
Sound.SC3.Plot.plot_ugen 0.1 (mce2 (sinOsc AR 20 0) (lfCub AR 20 0))
