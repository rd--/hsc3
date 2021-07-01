-- https://swiki.hfbk-hamburg.de/MusicTechnology/899 (nv) [Line 41]
let n i =
      let f = 1.9 ** i / 128
          p = mceFill 2 (\_ -> pinkNoise ar)
          b = 4 ** lfNoise2 kr (1.2 ** i / 16)
      in bpf p (b * 300) 0.15 * (5 ** lfNoise2 ar f / (i + 8) * 20)
in splay (mixFill 15 n) 1 0.5 0 True

-- https://swiki.hfbk-hamburg.de/MusicTechnology/899 (nv) [Line 41] ; id
let n z i =
      let f = 1.9 ** i / 128
          p = mce2 (pinkNoiseId (z,'α') ar) (pinkNoiseId (z,'β') ar)
          b = 4 ** lfNoise2Id z kr (1.2 ** i / 16)
      in bpf p (b * 300) 0.15 * (5 ** lfNoise2Id z ar f / (i + 8) * 20)
in splay (mixFill_z 'γ' 15 n) 1 0.5 0 True
