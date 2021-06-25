-- moogLadder
let o = mix (lfSaw AR (mce2 120 180) 0 * 0.33)
    cf = linExp (lfCub KR 0.1 (0.5 * pi)) (-1) 1 180 8500
in X.moogLadder o cf 0.75

-- moogLadder
let n = dust 'α' AR 3
in X.moogLadder n 2000 (mouseY KR 0 1 Linear 0.2)

-- moogLadder ; event control
let f _ (g,_,y,z,o,rx,ry,p,_,_) =
      let f0 = midiCPS p
          f1 = f0 * (1 + y * 8)
          res = rx + ry
          env = lagUD g 0.05 (2 - y * 2) * (2 - y) * z
      in pan2 (X.moogLadder (lfSaw AR f0 0) f1 res) (o * 2 - 1) env
in mix (eventVoicer 16 f) * control KR "gain" 1

