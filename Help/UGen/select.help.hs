-- select
let n = 3/2
    a = mce [sinOsc AR 440 0, saw AR 440, pulse AR 440 0.1]
in select (lfSaw KR 1 0 * n + n) a * 0.1

-- select ; as sequencer
let n = 10
    a = mce [517, 403, 89, 562, 816, 107, 241, 145, 90, 224]
    c = n / 2
    f = select (lfSaw KR 0.5 0 * c + c) a
in saw AR f * 0.1

-- select ; i-rate
let a = mce [rand 'α' 110 220,rand 'β' 220 440,rand 'γ' 440 880]
in sinOsc AR (select (rand 'δ' 0 3) a) 0 * 0.1

-- select ; buffer segment player ; requires=buf
let nc = 2
    buf = control KR "buf" 0
    n_segments = control_m KR "nseg" 32 (1,64,"lin")
    sel = control_m KR "sel" 0 (0,3,"lin")
    buf_size = bufFrames KR buf
    segment_size = bufFrames KR buf / n_segments
    tr = impulse KR (n_segments / bufDur KR buf) 0
    phase = phasor AR tr (bufRateScale KR buf) 0 (segment_size - 1) 0
    ix = mce [dseries 'α' dinf 0 1 `modE` n_segments -- play segments in sequence (reconstruct)
             ,n_segments - 1 - (dseries 'β' dinf 0 1 `modE` n_segments) -- play segments in reverse sequence
             ,dbrown 'γ' dinf 0 (n_segments - 1) 1
             ,dwhite 'δ' dinf 0 (n_segments - 1)]
    zero = select sel (demand tr 0 (ix * segment_size))
in bufRdN nc AR buf (zero + phase) NoLoop * 0.1

---- ; setup ; nc=2
{buf = 0 ; fn = "/home/rohan/data/audio/amen.wav"}
withSC3 (async (b_allocRead buf fn 0 0))
