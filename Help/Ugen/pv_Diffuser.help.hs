-- pv_Diffuser ; trigger revised phase shifts with MouseX crossing center of screen
let b = control kr "buf" 100
    z = playBuf 1 ar b (bufRateScale kr b) 0 0 Loop DoNothing
    f = fft' (localBufId 'α' 2048 1) z
    x = mouseX kr 0 1 Linear 0.1
    h = pv_Diffuser f (x `greater_than` 0.5)
in mce2 z (ifft' h * 0.5)

---- ; load buf
let fn = "/usr/share/SuperCollider/sounds/a11wlk01.wav"
let fn = "/home/rohan/data/audio/instr/bosendorfer/064/C5.aif"
withSc3 (async (b_allocRead 0 fn 0 0))
