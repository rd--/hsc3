-- out ; oscillators at outputs zero (330hz) and one (331hz)
out 0 (sinOsc ar (mce2 330 331) 0 * 0.1)

-- out ; an out node is implicitly added by various hsc3 functions if not given
sinOsc ar (mce2 330 331) 0 * 0.1

-- out ; out is a summing output, see replaceOut for over-writing output
mrg [out 0 (sinOsc ar (mce2 330 990) 0 * 0.05)
    ,out 0 (sinOsc ar (mce2 331 991) 0 * 0.05)]

-- out ; summing (phase cancellation)
mrg [out 0 (sinOsc ar 440 0),out 0 (sinOsc ar 440 pi)]

-- out ; summing ; non phase cancellation
mce [sinOsc ar (mce [440,441]) (mce [0,pi]),sinOsc ar (mce [440,441]) (mce [pi,0])] * 0.1

-- out ; summing / transpose ; phase cancellation
mceTranspose (mce [sinOsc ar (mce [440,441]) (mce [0,pi]),sinOsc ar (mce [440,441]) (mce [pi,0])])

-- out ; multiple out ; channel layout is L=440,660 and R=441,661
let f = mce2 (mce2 440 660) (mce2 441 661)
in out 0 (sinOsc ar f 0 * 0.1)

-- out ; kr ; see drawings section below for UI
let freq = mce [1,2,3,5,7,9,11,13,17,19,23]
    mul = control kr "mul" 0.1
    amp = control kr "amp" 0.1
in out 12000 (sinOsc kr (freq * mul) 0 * amp)

-- out ; nested mce ; multiple out
out 0 (sinOsc ar (mce1 (mce2 110 220)) 0 * 0.1)

---- ; drawings
UI.ui_sc3_scope 12 12000 (2 ^ 14) 0 "control" 0
UI.ui_sc3_ctl_plot 900 11 12000 900 (1/25) 5
