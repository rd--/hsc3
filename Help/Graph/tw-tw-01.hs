-- tim walters ; <https://swiki.hfbk-hamburg.de/MusicTechnology/899>
-- this graph has multiple out UGens.
-- however, and oddly, summing them alters the sound...
let f k i =
      let x = impulse KR ((0.5 ** i) / k) 0
      in sinOsc AR i (sinOsc AR ((i + k) ** i) 0 / (decay x (mce2 i (i + 1)) * k))
    s = mixFill 16 (\k -> mixFill 6 (f k))
in gVerb s 1 3 0.5 0.5 15 1 0.7 0.5 300 / 512
