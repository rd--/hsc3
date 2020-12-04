-- http://www.fredrikolofsson.com/f0blog/?q=node/490 (f0)
let pkt_07_param n =
      let f g z = take n (iterate g z)
      in zip4 (f (* 4) 2) (f (/ 2) (1/2)) (f (* 4) 1) (f (* 4) 2)
    pkt_07_gen f0 (m,f1,l,r) = sinOsc AR f0 0 * m + linExp (sinOsc AR f1 0) (-1) 1 l r
    pkt_07 n =
      let c = foldl pkt_07_gen 1 (pkt_07_param n)
          o = sinOsc AR c 0 * 0.1
      in gVerb (leakDC o 0.995) 16 8 0.75 0.5 15 1 0.7 0.5 16 * 0.1
in pkt_07 12
