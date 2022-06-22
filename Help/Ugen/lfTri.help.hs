-- lfTri ; see <http://thread.gmane.org/gmane.comp.audio.supercollider.user/84719>
lfTri ar 500 1 * 0.1

-- lfTri ; used as both Oscillator and LFO
lfTri ar (lfTri kr 4 0 * 400 + 400) 0 * 0.1

-- lfTri ; multiple phases
let f = lfTri kr 0.4 (mce [0..3]) * 200 + 400
in mix (lfTri ar f 0 * 0.1)

-- lfTri ; mouse control
let x = midiCps (mouseX kr 20 72 Linear 0.2)
    e = xLine kr 0.01 1 20 DoNothing
    o1 = triAS 25 x * (1 - e)
    o2 = lfTri ar x 0 * e
in mce2 o1 o2 * 0.1

-- lfTri ; c.f. dpw3Tri fast sweeps
lfTri ar (mouseX kr 200 12000 Exponential 0.2) 0 * 0.1

-- lfTri ; c.f. dpw3Tri efficiency
let f = X.randNId 50 'α' 50 5000
in splay (lfTri ar f 0) 1 0.1 0 True

---- ; drawings
Sound.Sc3.Plot.plot_ugen1 0.1 (lfTri ar 40 0)
Sound.Sc3.Plot.plot_ugen1 0.1 (lfTri ar (xLine kr 1 800 0.1 DoNothing) 0)
