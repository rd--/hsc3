-- dxrand ; c.f. drand ; dxrand never plays the same value twice in a row
let i = mce [0.2,0.4,dseqId 'α' 2 (mce [0.1,0.1])]
    d = dxrandId 'β' dinf i
in tDuty ar d 0 DoNothing (dwhiteId 'γ' dinf 0.5 1) 0

-- dxrand
let i0 = mce [0.2,0.4,dseqId 'α' 2 (mce [0.1,0.1])]
    i1 = mceMap (* 0.5) i0
    i2 = i0 * 0.5
    d = dxrandId 'β' dinf i2 -- compare i1 & i2
in tDuty ar d 0 DoNothing (dwhiteId 'γ' dinf 0.5 1) 0

-- dxrand
let n = dxrandId 'α' dinf (mce [1, 3, 2, 7, 8])
    x = mouseX kr 1 400 Exponential 0.1
    t = impulse kr x 0
    f = demand t 0 n * 30 + 340
in sinOsc ar f 0 * 0.1

{---- ; note

The list inputs to demand rate ugens may operate at different rates.
The variants i' and i'' above ought to generate the same graph.  A
simple-minded mce rule sets the rate of the operator primitive to the
maximum rate of the inputs and then does not revise this after mce
transformation, where it may be lower.  The hsc3 constructors attempt
to get this right!
-}

