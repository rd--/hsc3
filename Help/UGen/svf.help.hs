-- svf
let signal = lfSaw AR (range 110 35 (lfSaw KR 2 0)) 0
    cutoff = mouseX KR 20 20000 Exponential 0.2
    res = mouseY KR 1 0 Linear 0.2
    k = control KR
    low = k "low" 0.1
    band = k "band" 0.0
    high = k "high" 0.0
    notch = k "notch" 0.0
    peak_ = k "peak" 0.0
in X.svf signal cutoff res low band high notch peak_

---- ; edit
msg k v = withSC3 (Sound.OSC.sendMessage (n_set1 (-1) k v))
msg "low" 0.1
msg "band" 0.1
msg "high" 0.0
msg "notch" 0.0
msg "peak" 0.0
