-- blitB3Tri
X.blitB3Tri AR (xLine KR 1000 20 10 DoNothing) 0.99 0.99 * 0.1

-- blitB3Tri ; aliasing at higher frequencies (over 5000Hz or so) is very beautiful in point scope
let x = mouseX KR 20 8000 Exponential 0.2
    y = mouseY KR 0.001 0.99 Linear 0.2
    z = X.blitB3Tri AR x 0.99 y * 0.1
in mce2 z (delayN z 0.2 (sampleDur * control_m KR "sampleDelay" 12 (0,64,"lin")))

-- c.f. lfTri ; more efficient, some aliasing from 3000hz ; less harmonics for lower fundamentals
let x = mouseX KR 20 8000 Exponential 0.2
in lfTri AR x 0 * 0.1

-- blitB3Tri ; event control
let f _ (g,x,y,z,o,rx,ry,_,_,_) =
      let freq = midiCPS (x * 12 + 48)
      in pan2 (X.blitB3Tri AR freq (1 - (rx * 0.05)) y) (o * 2 - 1) (z * g)
in mix (rEventVoicer 16 f) * control KR "gain" 0.25

---- ; drawings
UI.ui_sc3_scope 2 0 (2 ^ 14) 0 "audio" 2
