-- theremin (jmcc) ; mouse control
let m = 7
    detune = 0
    x = mouseX kr 0 0.6 Linear 0.2
    y = mouseY kr 4000 200 Exponential 0.8
    f = y + detune
    f' = f + f * sinOsc ar m 0 * 0.02
    a = sinOsc ar f' 0 * x
in pan2 a 0 1

-- theremin (jmcc) ; event control
let f (_,w,x,y,z,o,rx,_,_,_,_) =
      let freq = lag (linExp y 0 1 4000 200) 0.8
          a = sinOsc ar (freq + freq * sinOsc ar (4 + 3 * rx) 0 * 0.02) 0 * x * 0.6 * lag w 0.2
      in pan2 a (o * 0.25) (0.5 + z)
in mix (eventVoicer 16 f) * control kr "gain" 0.5
