-- pv_Split
let sz = 1024 * 2
    op = (-)
    z = soundIn 0
    c0 = fft' (localBuf 'α' sz 1) z
    (c1,c2) = X.pv_split c0 (localBuf 'β' sz 1)
in ifft' c1 `op` ifft' c2

-- pv_Split
let sz = 1024 * 16
    op = (+)
    z = soundIn 0
    c0 = fft' (localBuf 'α' sz 1) z
    (c1,c2) = X.pv_split c0 (localBuf 'β' sz 1)
in ifft' c1 `op` ifft' c2

-- pv_Split
let sz = 1024 * 16
    op = (-)
    z = lfClipNoise 'α' ar 100 * 0.1
    c0 = fft' (localBuf 'α' sz 1) z
    (c1,c2) = X.pv_split c0 (localBuf 'β' sz 1)
in ifft' c1 `op` ifft' c2

-- pv_Split ; pv_splita is a variant that allocates a local buffer, deriving the size from the input graph
let s = whiteNoise 'α' ar * 0.1
    c1 = ffta 'β' 2048 s 0.5 0 1 0
    (c2,c3) = X.pv_splita 'γ' c1
    c4 = pv_BrickWall c2 (-0.85)
    c5 = pv_BrickWall c3 0.45
in ifft c4 0 0 * 0.15 + ifft c5 0 0

-- pv_Split ; pv_splita works with external buffers as well
let b = control kr "buf" 0
    z = soundIn 0
    c1 = fft b z 0.5 0 1 0
    (c2,c3) = X.pv_splita 'γ' c1
    c4 = pv_BrickWall c2 (-0.85)
    c5 = pv_BrickWall c3 0.45
in ifft c4 0 0 * 0.15 + ifft c5 0 0

---- ; pv_Split ; leaving out pv_split gives an invalid graph (ie. this expression is an error)
let s = whiteNoise 'α' ar * 0.1
    c1 = ffta 'β' 2048 s 0.5 0 1 0
    c4 = pv_BrickWall c1 (-0.85)
    c5 = pv_BrickWall c1 0.45
in ifft c4 0 0 * 0.15 + ifft c5 0 0

{---- Variant of `PV_Copy` that returns both signal paths.

`PV_Copy` graphs have two linear orderings that are not equivalent.

... -> fft -> ... .-> pv_copy -> pv1 -> ifft -> ...
                  |
                  .-> pv2 -> ifft -> ...

`PV_Split` graphs also have two _equivalent_ linear orderings.

                              .-> pv1 -> ifft1 -> ...
... -> fft -> ... -> pv_split |
                              .-> pv2 -> ifft2 -> ...

-}
