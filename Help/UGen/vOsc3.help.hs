-- vOsc3 ; mouse selects buffer and set amplitude ; see vOsc help for setup
let n = 8
    b = control KR "tbl" 0
    x = mouseX KR b (b + n - 1) Linear 0.1
    y = mouseY KR 0.01 0.2 Exponential 0.2
    o1 = vOsc3 AR x 120 121 129
    o2 = vOsc3 AR x 119 123 127
in mce2 o1 o2 * y

-- vOsc3 ; event control ; see vOsc help for setup
let f _ (g,_,y,z,_,_,_,p) =
      let n = 8
          f0 = midiCPS p
          b = control KR "tbl" 0
          pos = linLin y 0 1 b (b + n - 1)
          o1 = vOsc3 AR pos f0 (f0 + 1) (f0 + 9)
          o2 = vOsc3 AR pos (f0 - 1) (f0 + 3) (f0 + 4)
      in mce2 o1 o2 * g * z
in mix (rEventVoicer 10 f) * control KR "gain" 0.25
