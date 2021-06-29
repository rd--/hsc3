-- dwgSoundBoard
let sig = decay2 (dust 'α' ar 4) 0.01 1
    sb inp mul =
      let c1 = control_m kr "c1" 20.0 (1,40,"lin")
          c3 = control_m kr "c3" 20.0 (1,40,"lin")
          mix = control_m kr "mix" 0.8 (0,1,"lin")
          d1 = 199.0 * mul
          d2 = 211.0 * mul
          d3 = 223.0 * mul
          d4 = 227.0 * mul
          d5 = 229.0 * mul
          d6 = 233.0 * mul
          d7 = 239.0 * mul
          d8 = 241.0 * mul
      in X.dwgSoundBoard inp c1 c3 mix d1 d2 d3 d4 d5 d6 d7 d8
in sb sig (mce2 1 (control_m kr "mul" 2.8 (0.25,4,"lin")))

-- dwgSoundBoard ; event control
let f c (g,x,y,z,o,rx,ry,_,_,_) =
      let env = decay2 (trig g controlDur * z) 0.01 (0.05 + y * 0.15)
          sig = pinkNoise c ar * env
          c1_min = control_m kr "c1_min" 5 (1,5,"lin")
          c3_min = control_m kr "c3_min" 5 (1,5,"lin")
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
in mix (eventVoicer 16 f) * control kr "gain" 1

-- dwgSoundBoard ; pluck ; event control
let f _ (g,x,y,z,o,rx,_,_,_,_) =
      let n = whiteNoise 'α' ar * (z + y * 0.75)
          dl_max = 1 / 220 -- 110
          dl = dl_max * (1 - x * 0.9)
          sig = pluck n g dl_max dl 10 (linLin y 0 1 0.65 0.80) -- 0.75 1.0
          flt = let m = y * 2
                in X.dwgSoundBoard sig 20 20 (rx * 0.35)
                   (199 * m) (211 * m) (223 * m) (227 * m) (229 * m) (233 * m) (239 * m) (241 * m)
      in pan2 flt (o * 2 - 1) 1
in mix (eventVoicer 16 f) * control kr "gain" 1
