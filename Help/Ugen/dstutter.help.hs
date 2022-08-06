-- dstutter
let inp = dseqId 'α' dinf (mce [1,2,3])
    nse = diwhiteId 'β' dinf 2 8
    rep = dstutterId 'γ' nse inp
    trg = impulse kr (mouseX kr 1 40 Exponential 0.2) 0
    frq = demand trg 0 rep * 30 + 340
in sinOsc ar frq 0 * 0.1

-- dstutter ; see also <https://www.listarc.bham.ac.uk/lists/sc-users/msg14775.html>
let a z = let xr = dxrandId z dinf (mce [0.1,0.2,0.3,0.4,0.5])
              lf = dstutterId z 2 xr
              du = duty ar lf 0 DoNothing lf
              tr = abs (hpz1 du) `greater_than` 0
              ph = sweep ar tr (1/du)
          in linExp ph 0 1 (randId z 50 100) (randId z 500 2000)
    f = mce (map a (id_seq 8 'α'))
    [s0,s1] = mceChannels (splay (sinOsc ar f 0) 1 1 0 True)
    o = limiter (rotate2 s0 s1 (lfSaw kr 0.1 0)) 1 0.01
in o * 0.1
