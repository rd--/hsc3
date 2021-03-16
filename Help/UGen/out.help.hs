-- out ; oscillators at outputs zero (330hz) and one (331hz)
out 0 (sinOsc AR (mce2 330 331) 0 * 0.1)

-- out ; an out node is implicitly added by various hsc3 functions if not given
sinOsc AR (mce2 330 331) 0 * 0.1

-- out ; out is a summing output, see replaceOut for over-writing output
mrg [out 0 (sinOsc AR (mce2 330 990) 0 * 0.05)
    ,out 0 (sinOsc AR (mce2 331 991) 0 * 0.05)]

-- out ; summing (phase cancellation)
mrg [out 0 (sinOsc AR 440 0),out 0 (sinOsc AR 440 pi)]

-- out ; summing ; non phase cancellation
mce [sinOsc AR (mce [440,441]) (mce [0,pi]),sinOsc AR (mce [440,441]) (mce [pi,0])]

-- out ; summing / transpose ; phase cancellation
mce (transpose [sinOsc AR (mce [440,441]) (mce [0,pi]),sinOsc AR (mce [440,441]) (mce [pi,0])])

-- out ; multiple out ; channel layout is L=440,660 and R=441,661
let f = mce2 (mce2 440 660) (mce2 441 661)
in out 0 (sinOsc AR f 0 * 0.1)
