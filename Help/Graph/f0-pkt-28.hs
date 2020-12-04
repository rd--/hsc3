-- http://www.fredrikolofsson.com/f0blog/?q=node/490 (f0)
let pkt_28_rnd n i =
      let a = lfSaw AR ((i + 1) * 5) 0 * 0.5 * pi
          b = sinOsc AR ((i + 1) * 0.001) 0 * 0.5
          c = max (lfSaw AR (0.2 + b) (i / n) * 0.4) 0
          d = sinOsc AR 0.03 (i + 1) * 0.5 + 1
          e = sinOsc AR (200 + i) 0 * d
          f = sinOsc AR 0.04 (i + 2) * 0.5 + 1
          g = sinOsc AR (400 + i) 0 * f
          h = sinOsc AR 0.05 (i + 3) * 0.5 + 1
          j = sinOsc AR (800 + i) 0 * h
          k = linExp i 0 (n - 1) 70 1500
          z = sinOsc AR k a * c * e * g * j
          l = linLin i 0 (n - 1) (-0.925) 0.925
      in pan2 z l (1 / n)
    y = limiter (leakDC (mixFill 28 (pkt_28_rnd 28)) 0.995) 1 0.01
in gVerb y 3 5 0.2 0.8 20 0.1 0.7 0.5 300 * 0.2
