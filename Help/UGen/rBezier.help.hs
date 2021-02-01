-- rBezier ; reference sine tone
sinOsc AR 440 0 * 0.25

-- rBezier ; bezier approximation of sin function
X.rBezier AR 100 0.0001 440 0 (mce [0,0,0.2,-1.3,0.3,-1.3,0.5,0,0.7,1.3,0.8,1.3,1,0]) * 0.1

-- rBezier
let x = mouseX KR 40 4000 Exponential 0.2
in X.rBezier AR 100 0.0001 x 0 (mce [0,0,0.2,-1.3,0.3,-1.3,0.5,0,0.7,1.3,0.8,1.3,1,0]) * 0.1

-- rBezier
let x = mouseX KR 40 4000 Exponential 0.2
    y = mouseY KR 0.1 0.2 Linear 0.2
in X.rBezier AR 100 0.0001 x 0 (mce [0,0,y,-1.3,0.3,-1.3,0.5,0,0.7,1.3,0.8,1.3,1,0]) * 0.1

-- rBezier ; dynamic shifting of control points, oscillator
let rt = AR
    (f0,f1) = (200,205)
    f2 = 2
    w z l r = range l r (lfdNoise3 z rt f2)
    s = X.rBezier rt 100 0.0001 (w 'α' f0 f1) 0 (mce [0,0,w 'β' 0.05 0.25,-1.3,w 'γ' 0.25 0.45,-1.3
                                                     ,0.5,0,w 'δ' 0.55 0.75,1.3,w 'ε' 0.75 0.95,1.3
                                                     ,1,0])
in pan2 s (iRand 'α' (-1) 1) 0.1

-- rBezier ; dynamic shifting of control points, amplitude modulator
let rt = KR
    (f0,f1) = (0.15,16)
    f2 = 0.5
    w z l r = range l r (lfdNoise3 z rt f2)
    s = X.rBezier rt 100 0.0001 (w 'α' f0 f1) 0 (mce [0,0,w 'β' 0.05 0.25,-1.3,w 'γ' 0.25 0.45,-1.3
                                                     ,0.5,0,w 'δ' 0.55 0.75,1.3,w 'ε' 0.75 0.95,1.3
                                                     ,1,0])
in soundIn 0 * range 0.25 1 s

-- rBezier ; event control
let f _ (g,_,y,z,o,rx,ry,p) =
      let w i l r = linLin i 0 1 l r
          s = X.rBezier AR 20 0.004 (midiCPS p) 0 (mce [0,0,w y 0.05 0.25,-1.3,w rx 0.25 0.45,-1.3
                                                         ,0.5,0,w ry 0.55 0.75,1.3,w o 0.75 0.95,1.3
                                                         ,1,0])
      in pan2 s (o * 2 - 1) (z * lagUD g 0 1)
in mix (rEventVoicer 16 f) * control KR "gain" 0.5

---- ; drawings
import Sound.SC3.Plot {- hsc3-plot -}
let u = X.rBezier AR 440 0 (mce [0,0,0.2,-1.3,0.3,-1.3,0.5,0,0.7,1.3,0.8,1.3,1,0]) * 0.1
plot_ugen_nrt (48000,64) 0.012 u
plot_ugen1 0.01 u

---- ; see hcg-minus:Data.CG.Minus.Bezier for rendering bezier curves as wavetable
