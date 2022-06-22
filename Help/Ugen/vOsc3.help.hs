-- vOsc3 ; mouse selects buffer and set amplitude ; see vOsc help for setup
let n = 8
    b = control kr "tbl" 0
    x = mouseX kr b (b + n - 1) Linear 0.1
    y = mouseY kr 0.01 0.2 Exponential 0.2
    o1 = vOsc3 ar x 120 121 129
    o2 = vOsc3 ar x 119 123 127
in mce2 o1 o2 * y

-- vOsc3 ; event control ; see vOsc help for setup
let f (_,g,_,y,z,_,_,_,p,_,_) =
      let n = 8
          f0 = unitCps p
          b = control kr "tbl" 0
          pos = linLin y 0 1 b (b + n - 1)
          o1 = vOsc3 ar pos f0 (f0 + 1) (f0 + 9)
          o2 = vOsc3 ar pos (f0 - 1) (f0 + 3) (f0 + 4)
      in mce2 o1 o2 * g * z
in mix (eventVoicer 16 f) * control kr "gain" 0.25
