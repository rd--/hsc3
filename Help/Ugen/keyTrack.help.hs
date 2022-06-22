-- keyTrack
let s = soundIn (mce2 0 1)
    t = fft' (localBufId 'α' 1 4096) (mix s)
    k = keyTrack kr t 2.0 0.5
    p = poll (impulse kr 1 0) k 0 (label "key")
in mrg2 s p
