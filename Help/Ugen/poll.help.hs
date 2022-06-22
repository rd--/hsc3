-- poll
let t = impulse kr 10 0
    l = line kr 0 1 1 RemoveSynth
in poll t l 0 (label "polling...")

-- poll ; multichannel expansion (requires labels be equal length...)
let t = impulse kr (mce2 10 5) 0
    l = line kr 0 (mce2 1 5) (mce2 1 2) DoNothing
in poll t l 0 (mce2 (label "t1") (label "t2"))

-- poll ; will not poll once with a trigger of one, use impulse with frequency zero
let k = control kr "k" 0.3
    x = negate (k * 1.1)
    t = impulse kr 0 0 {- 1 -}
in mrg2 x (poll t x (-1) (label "x"))

-- poll ; at trigger control
let t = trigControl "t" 0.3
    f1 = lfNoise2Id 'α' ar 0.25 * 100 + 110
    f2 = lfNoise2Id 'β' ar 0.25 * 200 + 220
    s = gendy1Id 'γ' ar 1 1 1 1 f1 f2 0.5 0.5 12 0 * 0.1
    p = poll t (mce2 f1 f2) (-1) (mce2 (label "f1") (label "f2"))
in mrg2 s p

-- poll ; print oscillator frequency
let x = mouseX kr 200 260 Exponential 0.2
    o = sinOsc ar x 0 * 0.25
    t = impulse kr 2 0
in mrg2 o (poll t x 0 (label "polling..."))

---- ; set t control
withSC3 (Sound.OSC.sendMessage (n_set1 (-1) "t" 1))
