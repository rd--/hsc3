-- ; analogue daze (jmcc) #3
let dsequ k s tr = demand tr 0 (dseq k dinf (mce s))
    patternList = [55,63,60,63,57,65,62,65]
    f k octave clockRate pwmrate fltrate =
        let trg = impulse KR clockRate 0
            freq = map (midiCPS .  (+ (12 * octave))) patternList
            sq = dsequ 'α' freq trg
            pwm = sinOsc KR pwmrate (rand (k,'β') 0 (2 * pi)) * 0.4 + 0.5
            cf = sinOsc KR fltrate (rand (k,'γ') 0 (2 * pi)) * 1400 + 2000
        in rlpf (lfPulse AR (lag sq 0.05) 0 pwm * 0.1) cf (1/15)
    a = lfNoise0 'δ' AR (lfNoise1 'ε' KR 0.3 * 6000 + 8000) * (mce2 0.07 0.08)
    x = decay (impulse AR 2 0) 0.15 * a
    g = mce [f 'ζ' 1 8 0.31 0.2,f 'η' 0 2 0.13 0.11] + x
    z = 0.4 * (combN g 0.375 0.375 5 + mceReverse g)
    e = envLinen 2 56 2 1
in z * envGen KR 1 1 0 1 RemoveSynth e
