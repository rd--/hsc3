-- http://sccode.org/1-L (jl)
let urangeM :: UGen -> UGen -> UGen
    urangeM u = let [u0,u1] = mceChannels u in urange u0 u1
    n = mce [400,500,450,376,600]
    d = 1/20
    f = demand (impulse kr d (mce2 0 0.5)) 0 (dseq dinf n)
    f_ = urangeM f (lfPulse kr 8 0 (range 0 1 (lfTri kr d 1)))
in lpf (saw ar (max 376 f_ * mce2 1 0.99) * 0.5) 8000 * 0.1

-- http://sccode.org/1-L (jl) ; id
let urangeM :: UGen -> UGen -> UGen
    urangeM u = let [u0,u1] = mceChannels u in urange u0 u1
    n = mce [400,500,450,376,600]
    d = 1/20
    f = demand (impulse kr d (mce2 0 0.5)) 0 (dseqId 'α' dinf n)
    f_ = urangeM f (lfPulse kr 8 0 (range 0 1 (lfTri kr d 1)))
in lpf (saw ar (max 376 f_ * mce2 1 0.99) * 0.5) 8000 * 0.1
