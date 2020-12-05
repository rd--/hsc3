-- seqr (rd)
let nrand n e l = mceChannels . X.randN n e l
    nfreq z n l r = map (midiCPS . floorE) (nrand n z l r)
    seqr_f f e =
      let n = constant (length e `div` 2)
      in select (lfSaw KR f 0 * n + n) (mce e)
    seqr n =
      let f = rand 'α' 9 18 / constant n
          fr = mce2 (seqr_f f (nfreq 'β' n 72 96)) (seqr_f f (nfreq 'γ' n 72 84))
          nh = mce2 (seqr_f f (nrand n 'δ' 1 3)) (seqr_f f (nrand n 'ε' 3 6))
          b = blip AR fr nh
          a = mce2 (seqr_f f (nrand n 'ζ' 0.05 0.10)) (seqr_f f (nrand n 'η' 0.05 0.15))
      in b * a
in seqr 12
