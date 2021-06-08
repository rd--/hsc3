-- http://www.fredrikolofsson.com/f0blog/?q=node/490 (f0)
let n = 8
    pkt i =
      let t = i / constant n
          a = abs (varSaw AR 0.02 t 0.5 * 7.5)
          b = varSaw AR 0.16 t (2/3) * a + 300
          c = varSaw AR 0.064 t 0.5 * 25 + 50
          d = varSaw AR 0.012 t 0.75 * c + 200
          e = varSaw AR 0.024 t 0.25 * 0.475 + 0.5
          f = varSaw AR (100 + i) t e
          g = varSaw AR 0.048 0 0.5 * 25 + 150
          h = varSaw AR ((i + 1) * g) t (1/3) * 150
          o = sinOsc AR h (f * pi) * d + b
          z = leakDC (varSaw AR o t 0.5) 0.995
      in pan2 z (varSaw AR 0.02 t 0.5) 1
in mixFill n pkt * (0.25 / constant n)
