-- gend1 ; sc3 default parameters
gendy1 'α' AR 1 1 1 1 440 660 0.5 0.5 12 12 * 0.15

-- gendy1 ; wandering bass
gendy1 'α' AR 1 1 1.0 1.0 30 100 0.3 0.05 5 5 * 0.15

-- gendy1 ; play me
let x = mouseX KR 100 1000 Exponential 0.1
    g = gendy1 'α' AR 1 1 1.0 1.0 30 100 0.3 0.05 5 5
in rlpf g 500 0.3 * 0.1

-- gendy1 ; scream
let x = mouseX KR 220 440 Exponential 0.1
    y = mouseY KR 0.0 1.0 Linear 0.1
in gendy1 'α' AR 2 3 1 1 x (8 * x) y y 7 7 *0.3

-- gendy1 ; 1 CP = random noise
gendy1 'α' AR 1 1 1 1 440 660 0.5 0.5 1 1 * 0.15

-- gendy1 ; 2 CPs = an oscillator
gendy1 'α' AR 1 1 1 1 440 660 0.5 0.5 2 2 * 0.15

-- gendy1 ; used as an LFO
let ad = sinOsc KR 0.10 0 * 0.49 + 0.51
    dd = sinOsc KR 0.13 0 * 0.49 + 0.51
    as = sinOsc KR 0.17 0 * 0.49 + 0.51
    ds = sinOsc KR 0.19 0 * 0.49 + 0.51
    g = gendy1 'α' KR 2 4 ad dd 3.4 3.5 as ds 10 10
in sinOsc AR (g * 50 + 350) 0 * 0.3

-- gendy1 ; wasp
let ad = sinOsc KR 0.1 0 * 0.1 + 0.9
in gendy1 'α' AR 0 0 ad 1.0 50 1000 1 0.005 12 12 * 0.2

-- gendy1 ; modulate distributions ; distributions change the duration structure and spectrum
let x = mouseX KR 0 7 Linear 0.1
    y = mouseY KR 0 7 Linear 0.1
in gendy1 'α' AR x y 1 1 440 660 0.5 0.5 12 12 * 0.2

-- gendy1 ; modulate number of CPs
let x = mouseX KR 1 13 Linear 0.1
in gendy1 'α' AR 1 1 1 1 440 660 0.5 0.5 12 x * 0.2

-- gendy1 ; self modulation
let x = mouseX KR 1   13 Linear 0.1
    y = mouseY KR 0.1 10 Linear 0.1
    g0 = gendy1 'α' AR 5 4 0.3 0.7 0.1 y 1.0 1.0 5 5
in gendy1 'α' AR 1 1 1 1 440 (g0 * 500 + 600) 0.5 0.5 12 x * 0.2

-- gendy1 ; use SINUS to track oscillator and take CP positions from it ; use adParam and ddParam
let p = lfPulse KR 100 0 0.4
    s = sinOsc KR 30 0 * 0.5
in gendy1 'α' AR 6 6 p s 440 660 0.5 0.5 12 12 * 0.2

-- gendy1 ; near the corners are interesting
let x = mouseX KR 0 200 Linear 0.1
    y = mouseY KR 0 200 Linear 0.1
    p = lfPulse KR x 0 0.4
    s = sinOsc KR y 0 * 0.5
in gendy1 'α' AR 6 6 p s 440 660 0.5 0.5 12 12 * 0.2

-- gendy1 ; texture
let node e = let f = rand e 130 160.3
                 r0 = rand ('α',e) 0 6
                 r1 = rand ('β',e) 0 6
                 l = rand e (-1) 1
                 ad = sinOsc KR 0.10 0 * 0.49 + 0.51
                 dd = sinOsc KR 0.13 0 * 0.49 + 0.51
                 as = sinOsc KR 0.17 0 * 0.49 + 0.51
                 ds = sinOsc KR 0.19 0 * 0.49 + 0.51
                 g = gendy1 'γ' AR r0 r1 ad dd f f as ds 12 12
                 o = sinOsc AR (g * 200 + 400) 0
             in pan2 o l 0.1
in mix (mce (map node (id_seq 10 'δ')))

-- gendy1 ; try durscale 10 and 0 too
let x = mouseX KR 10 700 Linear 0.1
    y = mouseY KR 50 1000 Linear 0.1
    g = gendy1 'α' AR 2 3 1 1 1 x 0.5 0.1 10 10
in combN (resonz g y 0.1) 0.1 0.1 5 * 0.6
