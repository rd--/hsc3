-- pebble beach (np) ; http://sccode.org/1-u ; requires 'scsynth -u 57110 -m 32768'
let bg_f k =
      let sc = 0.4 + (k * 0.07)
          fr = range 3100 4900 (lag (lfNoise0 kr 21.8) 0.7) * sc
          am = range 0 1 (lag (lfNoise1 kr 5.227) 5.374)
      in bpf (pinkNoise ar * 0.4) fr 0.1 * am
    fg_f df tf _ =
      let d = dust ar (df * rand 0.8 1.2) * 50
          l = exprange 800 900 (sinOsc kr 2.2 0)
          r = exprange 2600 2900 (sinOsc kr 5.228 0)
          z = resonz d (tRand l r d) (tRand 0.03 0.08 d)
          t = lagUD (range 2 0.5 (saw kr tf)) 0.6 2.8
          o = z * t
      in o + combL o 0.8 (rand 0.2 0.8) (lchoose [-4,4])
    pebble_beach _ =
      let bg' = let am = range 0 1 (lag (lfNoise0 kr 34) 1.4)
                in brownNoise ar * 0.06 * am
          bg = bg' + mixFill 20 bg_f * 0.6
          tf = range 0.122 0.24 (sinOsc kr 0.17 0)
          df = exprange 1 700 (lfTri kr tf 0) * exprange 1 0.2 (lfTri kr tf 0)
          fg = mixFill 50 (fg_f df tf) * 0.2
      in (bg + fg) * line kr 0 1 1.2 DoNothing
in mce2 (pebble_beach ()) (pebble_beach ())

-- pebble beach (np) ; http://sccode.org/1-u ; requires 'scsynth -u 57110 -m 32768' ; id
let bg_f :: UGen -> UGen
    bg_f k =
      let sc = 0.4 + (k * 0.07)
          fr = range 3100 4900 (lag (lfNoise0Id 'α' kr 21.8) 0.7) * sc
          am = range 0 1 (lag (lfNoise1Id 'α' kr 5.227) 5.374)
      in bpf (pinkNoiseId 'α' ar * 0.4) fr 0.1 * am
    fg_f :: UGen -> UGen -> UGen
    fg_f df tf =
      let d = dustId 'α' ar (df * randId 'α' 0.8 1.2) * 50
          l = exprange 800 900 (sinOsc kr 2.2 0)
          r = exprange 2600 2900 (sinOsc kr 5.228 0)
          z = resonz d (tRandId 'α' l r d) (tRandId 'α' 0.03 0.08 d)
          t = lagUD (range 2 0.5 (saw kr tf)) 0.6 2.8
          o = z * t
      in o + combL o 0.8 (randId 'α' 0.2 0.8) (lchooseId 'α' [-4,4])
    pebble_beach :: ID a => a -> UGen
    pebble_beach j =
      let bg' = let am = range 0 1 (lag (lfNoise0Id 'α' kr 34) 1.4)
                in brownNoiseId 'α' ar * 0.06 * am
          bg = bg' + sum (Protect.uprotect_seq (const False) j (map bg_f [0..19])) * 0.6
          tf = range 0.122 0.24 (sinOsc kr 0.17 0)
          df = exprange 1 700 (lfTri kr tf 0) * exprange 1 0.2 (lfTri kr tf 0)
          fg = mix (Protect.uclone_all j 50 (fg_f df tf)) * 0.2
      in (bg + fg) * line kr 0 1 1.2 DoNothing
in mce2 (pebble_beach 'α') (pebble_beach 'β')
