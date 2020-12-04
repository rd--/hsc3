-- pebble beach (np) ; http://sccode.org/1-u ; requires 'scsynth -u 57110 -m 32768'
let bg_f :: UGen -> UGen
    bg_f k =
      let sc = 0.4 + (k * 0.07)
          fr = range 3100 4900 (lag (lfNoise0 'α' KR 21.8) 0.7) * sc
          am = range 0 1 (lag (lfNoise1 'α' KR 5.227) 5.374)
      in bpf (pinkNoise 'α' AR * 0.4) fr 0.1 * am
    fg_f :: UGen -> UGen -> UGen
    fg_f df tf =
      let d = dust 'α' AR (df * rand 'α' 0.8 1.2) * 50
          l = exprange 800 900 (sinOsc KR 2.2 0)
          r = exprange 2600 2900 (sinOsc KR 5.228 0)
          z = resonz d (tRand 'α' l r d) (tRand 'α' 0.03 0.08 d)
          t = lagUD (range 2 0.5 (saw KR tf)) 0.6 2.8
          o = z * t
      in o + combL o 0.8 (rand 'α' 0.2 0.8) (lchoose 'α' [-4,4])
    pebble_beach :: ID a => a -> UGen
    pebble_beach j =
      let bg' = let am = range 0 1 (lag (lfNoise0 'α' KR 34) 1.4)
                in brownNoise 'α' AR * 0.06 * am
          bg = bg' + sum (Protect.uprotect_seq (const False) j (map bg_f [0..19])) * 0.6
          tf = range 0.122 0.24 (sinOsc KR 0.17 0)
          df = exprange 1 700 (lfTri KR tf 0) * exprange 1 0.2 (lfTri KR tf 0)
          fg = mix (Protect.uclone_all j 50 (fg_f df tf)) * 0.2
      in (bg + fg) * line KR 0 1 1.2 DoNothing
in mce2 (pebble_beach 'α') (pebble_beach 'β')
