-- https://twitter.com/redFrik/status/1136928201886904320 (f0)
let gt = greater_than
    f a i =
      let b = mce [4,2,1,3,5]
          c = roundTo (a ar (1/b) 0 * b) (a ar (b/9) 0 `gt` 0)
          o = sinOscFB ar (c ** 2 * ((a ar 0.02 i `gt` 0) + 1 * 50)) (a ar (c/9) 0 `modE` 1)
          s = o * max (a ar (1/b * a ar (b/99) i) 0) 0 * a ar 0.01 i
          x = allpassC s 1 (wrap c 0.5 1) 2
          y = bpf x (i * 99 + 400) 0.001 * (a ar 0.04 i + 1 * 9)
      in splay (x + y) 1 1 0 True / 3
in sum (zipWith f [lfTri,lfSaw,lfPar] [0,1,2]) / 3
