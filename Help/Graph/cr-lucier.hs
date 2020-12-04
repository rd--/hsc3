-- Alvin Lucier, "Music on a Long Thin Wire, Simulated" (Chandrasekhar Ramakrishnan)
let freq = control KR "freq" 60
    block_size = recip controlRate
    mk_dt f = recip f - block_size
    string_delay = mk_dt freq
    pk1_pos = 0.1 -- pickup one position
    src_pos = 0.3 -- source position
    pk2_pos = 0.9 -- pickup two postion
    max_delay = 1.0 -- maximum delay time (corresponds to a length = c * s)
    mk_delay i r = lpz1 (delayC i max_delay (r * string_delay))
    mk_allpass i r dt = lpz1 (allpassC i max_delay (r * string_delay) dt)
    drv = localIn 1 AR 0 -- driver (source + data stored in the string)
    pk1_R =
        let i = drv
            r = src_pos - pk1_pos
        in mk_delay i r
    pk1_L =
        let i = pk1_R * negate 1
            r = pk1_pos * 2
            dt = rand 'α' 0.001 0.11
        in mk_allpass i r dt
    pk2_L =
        let i = pk1_L
            r = pk2_pos - pk1_pos
        in mk_delay i r * 0.99
    stringL =
        let i = pk2_L
            r = 1.0 - pk2_pos
        in mk_delay i r
    pk2_R =
        let i = stringL * negate 1
            r = 1.0 - pk2_pos
            dt = 2 + rand 'β' 0.001 0.11
        in mk_allpass i r dt * 0.99
    stringR =
        let i = pk2_R
            r = pk2_pos - src_pos
        in mk_delay i r
    source =
        let s = sinOsc AR 220 0 * 0.01
            p = pulse AR (60 + amplitude KR drv 0.01 0.01 * 11) 0.5 * 0.1
            f = rlpf (s + p) 320 0.05
            e = 1.0 - min (amplitude KR drv 0.01 0.01) 1.0
        in normalizer f 0.7 0.01 * e
    l_out = localOut (source * 0.2 + stringR)
    outL = pk1_L + pk1_R
    outR = pk2_L + pk2_R
in mrg [mce2 outL outR, drv, source, l_out]
