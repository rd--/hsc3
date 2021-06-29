-- select
let n = 3/2
    a = mce [sinOsc ar 440 0, saw ar 440, pulse ar 440 0.1]
in select (lfSaw kr 1 0 * n + n) a * 0.1

-- select ; as sequencer
let n = 10
    a = mce [517, 403, 89, 562, 816, 107, 241, 145, 90, 224]
    c = n / 2
    f = select (lfSaw kr 0.5 0 * c + c) a
in saw ar f * 0.1

-- select ; i-rate
let a = mce [rand 'α' 110 220,rand 'β' 220 440,rand 'γ' 440 880]
in sinOsc ar (select (rand 'δ' 0 3) a) 0 * 0.1

-- select ; buffer segment player ; requires=buf
let nc = 2
    buf = control kr "buf" 0
    n_segments = control_m kr "nseg" 32 (1,64,"lin")
    sel = control_m kr "sel" 0 (0,3,"lin")
    buf_size = bufFrames kr buf
    segment_size = bufFrames kr buf / n_segments
    tr = impulse kr (n_segments / bufDur kr buf) 0
    phase = phasor ar tr (bufRateScale kr buf) 0 (segment_size - 1) 0
    ix = mce [dseries 'α' dinf 0 1 `modE` n_segments -- play segments in sequence (reconstruct)
             ,n_segments - 1 - (dseries 'β' dinf 0 1 `modE` n_segments) -- play segments in reverse sequence
             ,dbrown 'γ' dinf 0 (n_segments - 1) 1
             ,dwhite 'δ' dinf 0 (n_segments - 1)]
    zero = select sel (demand tr 0 (ix * segment_size))
in bufRdN nc ar buf (zero + phase) NoLoop * 0.1

---- ; setup ; nc=2
{buf = 0 ; fn = "/home/rohan/data/audio/amen.wav"}
withSC3 (async (b_allocRead buf fn 0 0))
