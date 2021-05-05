-- https://twitter.com/thormagnusson/status/463992770596577280 (tm) ; texture=overlap,60,15,3,inf
let f z i =
      let x = constant i + 1
          e = lfNoise2 z AR 0.5 * line AR 0 0.1 (rand z 1 99) DoNothing / (x * 0.2)
      in sinOsc AR (30 * x + linLin_b (lfNoise2 z AR 0.1) (-2) 2) 0 * e
in mceFill_z 'α' 2 (\z _ -> mixFill_z z 24 f)
