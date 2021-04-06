-- analogTape
let freq = control KR "freq" 110
    width = control KR "width" 0.5
    bias = control KR "bias" 0.5
    saturation = control KR "saturation" 0.5
    drive = control KR "drive" 0.5
    sig = varSaw AR freq 0 width
in mce2 (sig * 0.05) (X.analogTape sig bias saturation drive 1 0 * 0.1)

---- ; drawings
UI.ui_sc3_scope 2 0 (2 ^ 14) 0 "audio" 0
