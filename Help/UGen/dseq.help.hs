-- dseq ; at control rate
let n = dseqId 'α' 3 (mce [1,3,2,7,8])
    x = mouseX kr 1 40 Exponential 0.1
    t = impulse kr x 0
    f = demand t 0 n * 30 + 340
in sinOsc ar f 0 * 0.1

-- dseq ; at audio rate
let n = dseqId 'α' dinf (mce [1,3,2,7,8,32,16,18,12,24])
    x = mouseX kr 1 10000 Exponential 0.1
    t = impulse ar x 0
    f = demand t 0 n * 30 + 340
in sinOsc ar f 0 * 0.1

-- dseq ; the SC2 Sequencer UGen is somewhat like the sequ function below
let sequId e s tr = demand tr 0 (dseqId e dinf (mce s))
    t = impulse ar 6 0
    n0 = sequId 'α' [60,62,63,58,48,55] t
    n1 = sequId 'β' [63,60,48,62,55,58] t
in lfSaw ar (midiCPS (mce2 n0 n1)) 0 * 0.05

-- dseq ; rather than MCE expansion at tr, it can be clearer to view tr as a functor
let tr = impulse kr (mce [2,3,5]) 0
    f (z,t) = demand t 0 (dseqId z dinf (mce [60,63,67,69]))
    m = mce_map_ix f tr
    o = sinOsc ar (midiCPS m) 0 * 0.1
in splay o 1 1 0 True

-- dseq ; ln 2021-04-06 https://lukasnowok.github.io/spectrology/
let geom k z m = mce (take k (iterate (* m) z))
    d = demand (impulse ar 8 0) 0 (dseqId 'α' dinf (geom 9 1 1.25))
    e = xLine ar 1 0.7 20 DoNothing
in mix (sinOsc ar (geom 8 60 2 * d * e) 0) * 1/5 * 0.1

-- dseq ; mce
let d = demand (impulse ar 8 0) 0 (dseqId 'α' dinf (mce (map mce [[60,67],[59,62]])))
in sinOsc ar (midiCPS d) 0 * 0.1

-- dseq ; shared dseq, different patterns
uid_st_eval
(do a <- dseqM 3 (mce [1,3,2,7,8])
    let t = impulse kr 5 0
        f = demand t 0 (mce [a,a]) * 30 + 340
    return (sinOsc ar f 0 * 0.1))

-- dseq ; distinct dseq, equal patterns
uid_st_eval
(do a <- replicateM 2 (dseqM 3 (mce [1,3,2,7,8]))
    let t = impulse kr 5 0
        f = demand t 0 (mce a) * 30 + 340
    return (sinOsc ar f 0 * 0.1))

---- ; drawings
UI.ui_baudline 4096 50 "linear" 2
