-- bezier ; reference sine tone
sinOsc ar 440 0 * 0.1

-- bezier ; bezier approximation of sin function
X.bezier ar 100 0.0001 440 0 (mce [0,0,0.2,-1.3,0.3,-1.3,0.5,0,0.7,1.3,0.8,1.3,1,0]) * 0.1

-- bezier
let x = mouseX kr 40 4000 Exponential 0.2
in X.bezier ar 100 0.0001 x 0 (mce [0,0,0.2,-1.3,0.3,-1.3,0.5,0,0.7,1.3,0.8,1.3,1,0]) * 0.1

-- bezier
let x = mouseX kr 40 4000 Exponential 0.2
    y = mouseY kr 0.1 0.2 Linear 0.2
in X.bezier ar 100 0.0001 x 0 (mce [0,0,y,-1.3,0.3,-1.3,0.5,0,0.7,1.3,0.8,1.3,1,0]) * 0.1

-- bezier ; dynamic shifting of control points, oscillator
let rt = ar
    (f0,f1) = (200,205)
    f2 = 2
    wId z l r = range l r (lfdNoise3Id z rt f2)
    s = X.bezier rt 100 0.0001 (wId 'α' f0 f1) 0 (mce [0,0,wId 'β' 0.05 0.25,-1.3,wId 'γ' 0.25 0.45,-1.3
                                                     ,0.5,0,wId 'δ' 0.55 0.75,1.3,wId 'ε' 0.75 0.95,1.3
                                                     ,1,0])
in pan2 s (iRandId 'α' (-1) 1) 0.1

-- bezier ; dynamic shifting of control points, amplitude modulator
let rt = kr
    (f0,f1) = (0.15,16)
    f2 = 0.5
    wId z l r = range l r (lfdNoise3Id z rt f2)
    s = X.bezier rt 100 0.0001 (wId 'α' f0 f1) 0 (mce [0,0,wId 'β' 0.05 0.25,-1.3,wId 'γ' 0.25 0.45,-1.3
                                                     ,0.5,0,wId 'δ' 0.55 0.75,1.3,wId 'ε' 0.75 0.95,1.3
                                                     ,1,0])
in soundIn 0 * range 0.25 1 s

-- bezier ; event control
let f (_,g,_,y,z,o,rx,ry,p,px,_) =
      let w i l r = linLin i 0 1 l r
          freq = midiCps (p * 127 + leakDC px 0.995)
          s = X.bezier ar 20 0.004 freq 0 (mce [0,0,w y 0.05 0.25,-1.3,w rx 0.25 0.45,-1.3
                                                         ,0.5,0,w ry 0.55 0.75,1.3,w o 0.75 0.95,1.3
                                                         ,1,0])
      in pan2 s (o * 2 - 1) (z * lagUD g 0 (1 + y * 0.65))
in mix (voicer 16 f) * control kr "gain" 0.5

---- ; drawings
import Sound.SC3.Plot {- hsc3-plot -}
let u = X.bezier ar 440 0 (mce [0,0,0.2,-1.3,0.3,-1.3,0.5,0,0.7,1.3,0.8,1.3,1,0]) * 0.1
plot_ugen_nrt (48000,64) 0.012 u
plot_ugen1 0.01 u

---- ; see hcg-minus:Data.CG.Minus.Bezier for rendering bezier curves as wavetable
