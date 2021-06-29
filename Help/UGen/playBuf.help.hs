-- playBuf ; requires=buf ; once only
let nc = 2
    b = control kr "buf" 0
in playBuf nc ar b (bufRateScale kr b) 1 0 NoLoop RemoveSynth

-- playBuf ; requires=buf ; infinite loop
let nc = 2
    b = control kr "buf" 0
in playBuf nc ar b (bufRateScale kr b) 1 0 Loop DoNothing

-- playBuf ; requires=buf ; trigger playback at each pulse
let nc = 2
    b = control kr "buf" 0
    t = impulse kr 2 0
    s = bufRateScale kr 0
in playBuf nc ar b s t 0 NoLoop DoNothing

-- playBuf ; requires=buf ; trigger playback at each pulse (diminishing intervals)
let nc = 2
    b = control kr "buf" 0
    f = xLine kr 0.1 100 10 RemoveSynth
    t = impulse kr f 0
    s = bufRateScale kr b
in playBuf nc ar b s t 0 NoLoop DoNothing

-- playBuf ; requires=buf ; loop playback, accelerating pitch
let nc = 2
    b = control kr "buf" 0
    r = xLine kr 0.1 100 60 RemoveSynth
in playBuf nc ar b r 1 0 Loop DoNothing

-- playBuf ; requires=buf ; sine wave control of playback rate, negative rate plays backwards
let nc = 2
    b = control kr "buf" 0
    f = xLine kr 0.2 8 30 RemoveSynth
    r = fSinOsc kr f 0 * 3 + 0.6
    s = bufRateScale kr b * r
in playBuf nc ar b s 1 0 Loop DoNothing

-- playBuf ; requires=buf ; channel mismatch message in server log ; can acquire second channel (subsequent load)
let nc = 1
    b = control kr "buf" 0
in playBuf (nc + 1) ar b (bufRateScale kr b) 1 0 Loop DoNothing

-- playBuf ; requires=buf ; scan sequence of buffers
let n_buf = 61
    t = impulse kr 1 0
    b = mouseX kr 0 n_buf Linear 0.2
    r = bufRateScale kr b
in playBuf 1 ar b r t 0 Loop DoNothing * 0.5

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
