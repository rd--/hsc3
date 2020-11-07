-- dWGBowedTor
let k = control KR
    out_ = k "out" 0
    freq = k "freq" 440
    amp = k "amp" 0.5
    force = k "force" 1
    gate_ = k "gate" 1
    pos = k "pos" 0.07
    c1 = k "c1" 0.25
    c3 = k "c3" 31
    pan = k "pan" 0
    vib = gendy1 'α' KR 1 1 1 1 0.1 4 0.5 0.5 12 0 * 0.003 + 1
    s1 = X.dWGBowedTor AR (freq  *vib) amp force gate_ pos 0.1 c1 c3 0.55 2 5.2 1 3000 1.8
    s2 = X.dWGSoundBoard AR s1 20 20 0.8 199 211 223 227 229 233 239 241
    s3 = bpf s2 118 1 + s2
    s4 = bpf s3 430 1 + s3
    s5 = bpf s4 490 1 + s4
    s6 = lpf s5 6000
in out out_ (pan2 (s6 * 0.1) pan 1)
