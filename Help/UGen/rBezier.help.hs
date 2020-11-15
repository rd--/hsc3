-- rBezier ; reference sine tone
sinOsc AR 440 0 * 0.25

-- rBezier ; bezier approximation of sin function
X.rBezier AR 440 0 (mce [0,0,0.2,-1.3,0.3,-1.3,0.5,0,0.7,1.3,0.8,1.3,1,0]) * 0.1

-- rBezier
let x = mouseX KR 40 4000 Exponential 0.2
in X.rBezier AR x 0 (mce [0,0,0.2,-1.3,0.3,-1.3,0.5,0,0.7,1.3,0.8,1.3,1,0]) * 0.1

-- rBezier
let x = mouseX KR 40 4000 Exponential 0.2
    y = mouseY KR 0.1 0.2 Linear 0.2
in X.rBezier AR x 0 (mce [0,0,y,-1.3,0.3,-1.3,0.5,0,0.7,1.3,0.8,1.3,1,0]) * 0.1

-- rBezier ; dynamic shifting of control points, oscillator
let rt = AR
    (f0,f1) = (200,205)
    f2 = 2
    w z l r = range l r (lfdNoise3 z rt f2)
    s = X.rBezier rt (w 'α' f0 f1) 0 (mce [0,0,w 'β' 0.05 0.25,-1.3,w 'γ' 0.25 0.45,-1.3
                                          ,0.5,0,w 'δ' 0.55 0.75,1.3,w 'ε' 0.75 0.95,1.3
                                          ,1,0])
in pan2 s (iRand 'α' (-1) 1) 0.1

-- rBezier ; dynamic shifting of control points, amplitude modulator
let rt = KR
    (f0,f1) = (0.15,16)
    f2 = 0.5
    w z l r = range l r (lfdNoise3 z rt f2)
    s = X.rBezier rt (w 'α' f0 f1) 0 (mce [0,0,w 'β' 0.05 0.25,-1.3,w 'γ' 0.25 0.45,-1.3
                                          ,0.5,0,w 'δ' 0.55 0.75,1.3,w 'ε' 0.75 0.95,1.3
                                          ,1,0])
in soundIn 0 * range 0.25 1 s

---- ; drawings
import Sound.SC3.Plot {- hsc3-plot -}
let u = X.rBezier AR 440 0 (mce [0,0,0.2,-1.3,0.3,-1.3,0.5,0,0.7,1.3,0.8,1.3,1,0]) * 0.1
plot_ugen_nrt (48000,64) 0.012 u
plot_ugen1 0.01 u

---- ; see hcg-minus:Data.CG.Minus.Bezier for rendering bezier curves as wavetable