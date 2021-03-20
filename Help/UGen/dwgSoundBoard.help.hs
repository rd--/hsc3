-- dwgSoundBoard
let sig = decay2 (dust 'α' AR 4) 0.01 1
    sb inp mul =
      let c1 = control_md KR "c1" 20.0 (1,40,"lin",0,"")
          c3 = control_md KR "c3" 20.0 (1,40,"lin",0,"")
          mix = control_md KR "mix" 0.8 (0,1,"lin",0,"")
          d1 = 199.0 * mul
          d2 = 211.0 * mul
          d3 = 223.0 * mul
          d4 = 227.0 * mul
          d5 = 229.0 * mul
          d6 = 233.0 * mul
          d7 = 239.0 * mul
          d8 = 241.0 * mul
      in X.dwgSoundBoard inp c1 c3 mix d1 d2 d3 d4 d5 d6 d7 d8
in sb sig (mce2 1 (control_md KR "mul" 2.8 (0.25,4,"lin",0,"")))

-- dwgSoundBoard ; event control
let f c (g,x,y,z,o,rx,ry,_,_,_) =
      let env = decay2 (trig g controlDur * z) 0.01 (0.05 + y * 0.15)
          sig = pinkNoise c AR * env
          c1_min = control_md KR "c1_min" 5 (1,5,"lin",0,"")
          c3_min = control_md KR "c3_min" 5 (1,5,"lin",0,"")
          c1 = linLin rx 0 1 c1_min 20
          c3 = linLin ry 0 1 c3_min 20
          mix = linLin y 0 1 0.75 1
          mul = linLin (1 - x) 0 1 0.05 0.75
          d1 = 199.0 * mul
          d2 = 211.0 * mul
          d3 = 223.0 * mul
          d4 = 227.0 * mul
          d5 = 229.0 * mul
          d6 = 233.0 * mul
          d7 = 239.0 * mul
          d8 = 241.0 * mul
      in pan2 (X.dwgSoundBoard sig c1 c3 mix d1 d2 d3 d4 d5 d6 d7 d8) (o * 2 - 1) 1
in mix (rEventVoicer 16 f) * control KR "gain" 1
