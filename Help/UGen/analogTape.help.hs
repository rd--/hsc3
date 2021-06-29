-- analogTape
let freq = control kr "freq" 110
    width = control kr "width" 0.5
    bias = control kr "bias" 0.5
    saturation = control kr "saturation" 0.5
    drive = control kr "drive" 0.5
    sig = varSaw ar freq 0 width
in mce2 (sig * 0.05) (X.analogTape sig bias saturation drive 1 0 * 0.1)

-- analogTape ; event control
let bias = control kr "bias" 0.25
    saturation = control kr "saturation" 0.25
    drive = control kr "drive" 0.25
    f _ (g,x,y,z,o,rx,ry,_,_,_) =
      let freq = midiCPS (x * 12 + 48)
          width = y
          sig = varSaw ar freq 0 width
      in pan2 (X.analogTape sig (bias + rx) (saturation + ry) (drive + o) 1 0) (o * 2 - 1) (z * g)
in mix (eventVoicer 12 f) * control kr "gain" 1

---- ; drawings
UI.ui_sc3_scope 2 0 (2 ^ 14) 0.75 "audio" 0
