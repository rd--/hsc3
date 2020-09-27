import Sound.SC3 {- hsc3 -}

g_01 =
  let x = mouseX KR (-20) 20 Linear 0.2
      y = mouseY KR (-20) 20 Linear 0.2
  in sinOsc AR ((sinOsc KR 0.3 0 * 20) `lcmE` mce2 x y * 30 + 500) 0 * 0.1

{-

SC3: Least common multiple. This definition extends the usual
definition and returns a negative number if any of the operands is
negative. This makes it consistent with the lattice-theoretical
interpretation and its idempotency, commutative, associative,
absorption laws.

-}

h_01 = [lcm 4 6 == 12
       ,lcm 1 1 == 1
       ,lcm 1624 26 == 21112
       ,lcm 1624 (-26) /= (-21112)
       ,lcm (-1624) (-26) /= (-21112)
       ,lcm 513 (gcd 513 44) == 513]
