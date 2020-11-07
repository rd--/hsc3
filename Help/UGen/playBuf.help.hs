-- playBuf ; once only
let nc = 2
    b = control KR "buf" 0
in playBuf nc AR b (bufRateScale KR b) 1 0 NoLoop RemoveSynth

-- playBuf ; infinite loop
let nc = 2
    b = control KR "buf" 0
in playBuf nc AR b (bufRateScale KR b) 1 0 Loop DoNothing

-- playBuf ; trigger playback at each pulse
let nc = 2
    b = control KR "buf" 0
    t = impulse KR 2 0
    s = bufRateScale KR 0
in playBuf nc AR b s t 0 NoLoop DoNothing

-- playBuf ; trigger playback at each pulse (diminishing intervals)
let nc = 2
    b = control KR "buf" 0
    f = xLine KR 0.1 100 10 RemoveSynth
    t = impulse KR f 0
    s = bufRateScale KR b
in playBuf nc AR b s t 0 NoLoop DoNothing

-- playBuf ; loop playback, accelerating pitch
let nc = 2
    b = control KR "buf" 0
    r = xLine KR 0.1 100 60 RemoveSynth
in playBuf nc AR b r 1 0 Loop DoNothing

-- playBuf ; sine wave control of playback rate, negative rate plays backwards
let nc = 2
    b = control KR "buf" 0
    f = xLine KR 0.2 8 30 RemoveSynth
    r = fSinOsc KR f 0 * 3 + 0.6
    s = bufRateScale KR b * r
in playBuf nc AR b s 1 0 Loop DoNothing

-- playBuf ; channel mismatch message in server log ; can acquire second channel (subsequent load)
let nc = 1
    b = control KR "buf" 0
in playBuf (nc + 1) AR b (bufRateScale KR b) 1 0 Loop DoNothing

-- playBuf ; scan sequence of buffers
let n_buf = 61
    t = impulse KR 1 0
    b = mouseX KR 0 n_buf Linear 0.2
    r = bufRateScale KR b
in playBuf 1 AR b r t 0 Loop DoNothing * 0.5

---- ; setup ; nc=1
{buf = 0 ; fn = "/home/rohan/data/audio/metal.wav"}
withSC3 (async (b_allocRead buf fn 0 0))

---- ; setup ; nc=2
{buf = 0 ; fn = "/home/rohan/data/audio/pf-c5.aif"}
withSC3 (async (b_allocRead buf fn 0 0))

---- ; setup ; dir ; cnt=61
dir = "/home/rohan/data/audio/instr/celeste/long/"
dir_ls <- System.Directory.getDirectoryContents dir
fn_seq = zip [0..] (sort (filter ((== ".wav") . System.FilePath.takeExtension) dir_ls))
withSC3 (mapM_ async (map (\(ix,fn) -> b_allocRead ix (dir System.FilePath.</> fn) 0 0) fn_seq))
