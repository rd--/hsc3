-- laser bubble ; https://www.listarc.bham.ac.uk/lists/sc-users/msg14775.html
let n = 8
    laser_bubble_f :: ID a => a -> UGen
    laser_bubble_f z =
      let xr = dxrand z dinf (mce [0.1,0.2,0.3,0.4,0.5])
          lf = dstutter z 2 xr
          du = duty AR lf 0 DoNothing lf
          tr = abs (hpz1 du) `greater_than` 0
          ph = sweep tr (1/du)
      in linExp ph 0 1 (rand z 50 100) (rand z 500 2000)
    f = mce (map laser_bubble_f (take n ['α'..]))
    [s0,s1] = mceChannels (splay (sinOsc AR f 0) 1 1 0 True)
in limiter (rotate2 s0 s1 (lfSaw KR 0.1 0)) 1 0.01 * 0.25
