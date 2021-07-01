-- dbufwr ; monadic graph
uid_st_eval (do
  let b = asLocalBufId 'α' (replicate 24 210)
  s1 <- dseriesM 30 0 3
  s2 <- dseriesM 30 0 1
  s3 <- dseriesM 16 1 1
  s4 <- dwhiteM 8 1 16
  s5 <- dseqM dinf (mce2 s3 s4)
  wt <- dustM kr 1                  {- write trigger -}
  rp <- dseriesM dinf 0 1           {- read pointer -}
  wp <- dseqM dinf (mce2 s1 s2)     {- write pointer -}
  r <- dbufrdM b rp Loop            {- reader -}
  w <- dbufwrM b wp (s5 * 60) Loop  {- writer -}
  let d = demand wt 0 w
      f = lag (demand (impulse kr 16 0) 0 r) 0.01
      o = sinOsc ar (f * mce2 1 1.01) 0 * 0.1
  return (mrg [d, out 0 o]))

-- dbufwr ; uid graph
let b = asLocalBufId 'α' (replicate 24 210)
    s = dseqId 'β' dinf (mce2 (dseriesId 'γ' 16 1 1) (dwhiteId 'δ' 8 1 16))
    rp = dseriesId 'ε' dinf 0 1 {- read pointer -}
    wp = dseqId 'ζ' dinf (mce2 (dseriesId 'η' 30 0 3) (dseriesId 'θ' 30 0 1)) {- write pointer -}
    r = dbufrdId 'ι' b rp Loop {- reader -}
    w = dbufwrId 'κ' b wp (s * 60) Loop {- writer -}
    d = demand (dustId 'λ' kr 1) 0 w
    f = lag (demand (impulse kr 16 0) 0 r) 0.01
    o = sinOsc ar (f * mce2 1 1.01) 0 * 0.1
in mrg2 d (out 0 o)

-- dbufrd ; demand rate single memory recurrence relation ; simple counter
let rec1Id z k t f =
      let b = asLocalBufId z [k]
          r = dbufrdId z b 0 Loop {- reader -}
          w = dbufwrId z b 0 (f r) Loop {- writer -}
      in mrg2 (demand t 0 r) (demand t 0 w)
    f = rec1Id 'α' 0 (impulse kr 6 0) (\r -> (r + 1) `modE` 24)
in sinOsc ar (midiCPS (60 + f)) 0 * 0.1
