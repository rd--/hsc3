-- mouse clatter (rd, 2006-09-25)
let x = mouseX kr 100 12000 Linear 0.1
    y = mouseY kr 0.01 0.15 Linear 0.1
    n1 = lfNoise0 kr (mce [3,3.25])
    t = impulse kr (n1 * 16 + 18) 0
    n2 = tRand 0.005 y t
    n3 = whiteNoise ar
    n4 = tRand 10 x t
    n5 = tRand 0 1 t
    n6 = tExpRand 0.15 1 t
    o = let e = decay2 t 0.01 n2
        in bpf (n3 * e) n4 n5
    c = X.pv_SplitAlloc (fftAlloc 2048 o 0.5 0 1 0)
    n7 = pv_RandComb c n6 t
in mix (o * 0.05 + ifft n7 0 0)

-- mouse clatter (rd, 2006-09-25) ; id
let x = mouseX kr 100 12000 Linear 0.1
    y = mouseY kr 0.01 0.15 Linear 0.1
    n1 = lfNoise0Id 'α' kr (mce [3,3.25])
    t = impulse kr (n1 * 16 + 18) 0
    n2 = tRandId 'β' 0.005 y t
    n3 = whiteNoiseId 'γ' ar
    n4 = tRandId 'δ' 10 x t
    n5 = tRandId 'ε' 0 1 t
    n6 = tExpRandId 'ζ' 0.15 1 t
    o = let e = decay2 t 0.01 n2
        in bpf (n3 * e) n4 n5
    c = X.pv_SplitAllocId 'η' (fftAllocId 'θ' 2048 o 0.5 0 1 0)
    n7 = pv_RandCombId 'ι' c n6 t
in mix (o * 0.05 + ifft n7 0 0)
