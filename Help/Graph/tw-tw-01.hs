-- tim walters ; <https://swiki.hfbk-hamburg.de/MusicTechnology/899>
let f k i =
      let x = impulse kr ((0.5 ** i) / k) 0
      in sinOsc ar i (sinOsc ar ((i + k) ** i) 0 / (decay x (mce2 i (i + 1)) * k))
    s = mixFill 16 (\k -> mixFill 6 (f k))
in mix (mceTranspose (gVerb s 1 3 0.5 0.5 15 1 0.7 0.5 300 / 512))
