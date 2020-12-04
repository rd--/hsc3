-- data space (jmcc) #2 ; texture=overlap,1,6,4,inf
let r e = rand e 0
    p0 = lfPulse KR (r 'α' 200) 0 (r 'β' 1)
    p1 = lfPulse KR (r 'γ' 40) 0 (r 'δ' 1) * r 'ε' 8000 + r 'ζ' 2000
    p2 = lfPulse KR (r 'η' 20) 0 (r 'θ' 1)
    p3 = lfPulse KR (r 'ι' 4) 0 (r 'κ' 1) * r 'λ' 8000 + r 'μ'  2000
    p4 = lfPulse KR (r 'ν' 20) 0 (r 'ξ' 1)
    p5 = lfPulse KR (r 'ο' 4) 0 (r 'π' 1) * r 'ρ' 8000 + r 'σ'  2000
    f = p0 * p1 + p2 * p3 + p4 * p5
    dt = rand 'τ' 0.15 0.35
    o = lfPulse AR f 0 0.5 * 0.04
    l = lfNoise0 'υ' KR (r 'φ' 3) * 0.8
in combL (pan2 o l 1) dt dt 3
