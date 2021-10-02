-- svf ; state variable filter
let signal = lfSaw ar (range 110 35 (lfSaw kr 2 0)) 0
    cutoff = mouseX kr 20 20000 Exponential 0.2
    res = mouseY kr 1 0 Linear 0.2
    k = control kr
    low = k "low" 0.1
    band = k "band" 0.0
    high = k "high" 0.0
    notch = k "notch" 0.0
    peak_ = k "peak" 0.0
in X.svf signal cutoff res low band high notch peak_

-- svf ; event control
let f (c,g,x,y,z,o,rx,_,p,px,_) =
      let signal = lfSaw ar (midiCps (p + px)) 0
          cutoff = linExp x 0 1 20 20000
          res = y
          ty = constant c
          low = (ty + 0) `modE` 5 * z
          band = (ty + 1) `modE` 5 * z
          high = (ty + 2) `modE` 5 * z
          notch = (ty + 3) `modE` 5 * z
          peak_ = (ty + 4) `modE` 5 * z
      in pan2 (X.svf signal cutoff res low band high notch peak_) (o * 2 - 1) (lagUD g 0.1 (rx * 9))
in mix (eventVoicer 16 f) * control kr "gain" 0.2

---- ; control edits
msg k v = withSC3 (Sound.OSC.sendMessage (n_set1 (-1) k v))
msg "low" 0.0
msg "band" 0.0
msg "high" 0.0
msg "notch" 0.0
msg "peak" 0.1
