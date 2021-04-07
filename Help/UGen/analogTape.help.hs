-- analogTape
let freq = control KR "freq" 110
    width = control KR "width" 0.5
    bias = control KR "bias" 0.5
    saturation = control KR "saturation" 0.5
    drive = control KR "drive" 0.5
    sig = varSaw AR freq 0 width
in mce2 (sig * 0.05) (X.analogTape sig bias saturation drive 1 0 * 0.1)

-- analogTape ; event control
let bias = control KR "bias" 0.25
    saturation = control KR "saturation" 0.25
    drive = control KR "drive" 0.25
    f _ (g,x,y,z,o,rx,ry,_,_,_) =
      let freq = midiCPS (x * 12 + 48)
          width = y
          sig = varSaw AR freq 0 width
      in pan2 (X.analogTape sig (bias + rx) (saturation + ry) (drive + o) 1 0) (o * 2 - 1) (z * g)
in mix (rEventVoicer 12 f) * control KR "gain" 1

---- ; drawings
UI.ui_sc3_scope 2 0 (2 ^ 14) 0.75 "audio" 0
