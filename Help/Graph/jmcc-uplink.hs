-- uplink (jmcc) #2 ; texture=overlap,4,1,5,inf
let r = rand0
    p0 = lfPulse KR (r 'α' 20) 0 (r 'β' 1)
    p1 = lfPulse KR (r 'γ' 4) 0 (r 'δ' 1) * r 'ε' 8000 + r 'ζ' 2000
    f = mix (Protect.uclone_all 'η' 2 (p0 * p1))
in pan2 (lfPulse AR f 0 0.5 * 0.04) (rand 'θ' (-0.8) 0.8) 1
