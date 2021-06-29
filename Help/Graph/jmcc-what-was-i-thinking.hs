-- what was i thinking? (jmcc) #2
uid_st_eval (do
  n0 <- lfNoise1M kr 0.2
  n1 <- lfNoise1M kr 0.157
  let p = pulse ar f (n1 * 0.4 + 0.5) * 0.04
      i = lfPulse ar 0.1 0 0.05 * impulse ar 8 0 * 500
      d = decay i 2
      f = max (sinOsc kr 4 0 + 80) d
      z = rlpf p (n0 * 2000 + 2400) 0.2
      c x = do r <- randM 0 0.3
               n <- lfNoise1M kr r
               return (combL x 0.06 (n * 0.025 + 0.035) 1)
      y = z * 0.6
  z0 <- clone 2 (c y)
  z1 <- clone 2 (c y)
  return (z + mce [mix z0,mix z1]))
