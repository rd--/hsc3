-- mouse clatter (rd, 2006-09-25)
let x = mouseX KR 100 12000 Linear 0.1
    y = mouseY KR 0.01 0.15 Linear 0.1
    n1 = lfNoise0 'α' KR (mce [3,3.25])
    t = impulse KR (n1 * 16 + 18) 0
    n2 = tRand 'β' 0.005 y t
    n3 = whiteNoise 'γ' AR
    n4 = tRand 'δ' 10 x t
    n5 = tRand 'ε' 0 1 t
    n6 = tExpRand 'ζ' 0.15 1 t
    o = let e = decay2 t 0.01 n2
        in bpf (n3 * e) n4 n5
    c = X.pv_Splita 'η' (ffta 'θ' 2048 o 0.5 0 1 0)
    n7 = pv_RandComb 'ι' c n6 t
in mix (o * 0.05 + ifft n7 0 0)
