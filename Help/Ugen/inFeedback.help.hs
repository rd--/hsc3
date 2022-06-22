-- inFeedback ; audio feedback modulation
let f = inFeedback 1 0 * 1300 + 300
in sinOsc ar f 0 * 0.2

-- inFeedback ; the two graphs below can be started in either order and both tones will sound
out 0 (inFeedback 1 firstPrivateBus)

-- inFeedback
let b  = firstPrivateBus
    s0 = out b (sinOsc ar 220 0 * 0.1)
    s1 = out 0 (sinOsc ar 660 0 * 0.1)
in mrg [s0, s1]

-- inFeedback ; in (no feed-back)
let b = firstPrivateBus in in' 1 ar b

-- inFeedback ; resonator at 440hz, see localOut for variant
let b = firstPrivateBus
    p = inFeedback 1 b
    i = impulse ar 1 0
    d = delayC (i + (p * 0.995)) 1 (recip 440 - recip controlRate)
in mrg [offsetOut b d, offsetOut 0 p]

-- inFeedback ; compare resonator tone with oscillator (at right)
out 1 (sinOsc ar 440 0 * 0.1)
