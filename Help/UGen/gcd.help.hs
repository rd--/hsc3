import Sound.SC3 {- hsc3 -}

g_01 =
  let x = mouseX KR (-200) 200 Linear 0.2
      y = mouseY KR (-200) 200 Linear 0.2
  in sinOsc AR ((sinOsc KR 0.3 0 * 20) `gcdE` mce2 x y * 30 + 500) 0 * 0.1

{-

SC3: Greatest common divisor. This definition extends the usual
definition and returns a negative number if both operands are
negative. This makes it consistent with the lattice-theoretical
interpretation and its idempotency, commutative, associative,
absorption laws. <https://www.jsoftware.com/papers/eem/gcd.htm>

-}

h_01 =
  [gcd 4 6 == 2
  ,gcd 0 1 == 1
  ,gcd 1024 256 == 256
  ,gcd 1024 (-256) == 256
  ,gcd (-1024) (-256) /= (-256)
  ,gcd (-1024) (lcm (-1024) 256) /= (-1024)
  ,gcd 66 54 * lcm 66 54 == 66 * 54]
