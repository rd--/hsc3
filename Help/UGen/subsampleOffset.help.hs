-- subsampleOffset; impulse train that can be moved between samples
let i = impulse AR 2000 0
    z = 4
    o = (1 - subsampleOffset) + mouseX KR 0 (z - 1) Linear 0.1
    r = delayC i (sampleDur * z) (sampleDur * o)
in mrg2 (out 0 (pan2 i (-1) 0.1)) (offsetOut 0 (pan2 r 1 0.1))

---- ; drawings
UI.ui_sc3_scope 2 0 (2 ^ 14) 100 "audio" 0

{- Create two pulse trains and move one relative to the other.  When
cursor is at the left, the impulses are adjacent, on the right, they
are z samples apart.  View this with an oscilloscope. -}
