-- frac ; fractional part ; jrhb ; https://www.listarc.bham.ac.uk/lists/sc-users/msg68991.html
let x = mouseX KR (-10) 10 Linear 0.1
    y = mouseY KR 0 5 Linear 0.1
    i = (lfSaw KR 0.062 0 `in_range` (-10,10)) + (y * mce [0 .. 8])
    d = frac i
    a = 0.8 - modDif d 0.5 1
    z1 = gcdE i x + d
    z2 = lcmE i x + d
    freq = (abs (mceTranspose (mce [z1,z2])) + 1) * 120
in mix (sinOsc AR freq 0 * ampComp KR freq 261.625 (1/3)) * a * 0.01
