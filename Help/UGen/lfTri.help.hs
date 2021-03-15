-- lfTri ; see <http://thread.gmane.org/gmane.comp.audio.supercollider.user/84719>
lfTri AR 500 1 * 0.1

-- lfTri ; used as both Oscillator and LFO
lfTri AR (lfTri KR 4 0 * 400 + 400) 0 * 0.1

-- lfTri ; multiple phases
let f = lfTri KR 0.4 (mce [0..3]) * 200 + 400
in mix (lfTri AR f 0 * 0.1)

-- lfTri ; mouse control
let x = midiCPS (mouseX KR 20 72 Linear 0.2)
    e = xLine KR 0.01 1 20 DoNothing
    o1 = triAS 25 x * (1 - e)
    o2 = lfTri AR x 0 * e
in mce2 o1 o2 * 0.1

-- lfTri ; c.f. dpw3Tri fast sweeps
lfTri AR (mouseX KR 200 12000 Exponential 0.2) 0 * 0.1

-- lfTri ; c.f. dpw3Tri efficiency
let f = X.rRandN 50 'α' 50 5000
in splay (lfTri AR f 0) 1 0.1 0 True

---- ; drawings
Sound.SC3.Plot.plot_ugen1 0.1 (lfTri AR 40 0)
Sound.SC3.Plot.plot_ugen1 0.1 (lfTri AR (xLine KR 1 800 0.1 DoNothing) 0)
