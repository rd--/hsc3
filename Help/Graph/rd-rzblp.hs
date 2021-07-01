-- rzblp-u (rd)
let lfn f l r = range l r (lfNoise0 kr f)
    hpb q =
      let g _ = let f = lfn q 1330 1395
                    a = lfn q 0.001 0.007
                in blip ar f 24 * a
      in mixFill 2 g
    mk d =
      let q = lfn 5.2 5.2 5.21
          a = d dinf (mce [1,3,2,7,8])
          tr = impulse kr q 0
          freq = demand tr 0 a * 30 + 340
      in sum [blip ar freq 3 * lfn q 0.001 0.01
             ,resonz (impulse ar q (mce2 0 0))
               (lfn 5 30 640)
               (lfn q 0.1 0.5)
               * lfn q 0.01 1.8
             ,hpb q * lfn q 1.2 1.8
             ,blip ar (lfn q 16 36) 3 * mce2 0.03 0.09]
in mk drand + mk dxrandU
