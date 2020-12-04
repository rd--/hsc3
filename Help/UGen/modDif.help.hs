-- modDif
fSinOsc AR 440 0 * (modDif 0.2 (fSinOsc AR 2 0 * 0.5) 1)

-- modDif ; different moduli
let sig = lfSaw AR 10 0
    dist = modDif sig 0 (mce [0..8] * mouseX KR 0 (1/5) Linear 0.2)
in splay (sinOsc AR (dist * 4000 + 400) 0) 1 1 0 True * 0.1

-- modDif ; wrapping amplitude crossfade
let nc = 12
    nc_u = constant nc
    x = sinOsc AR (X.randN nc 'α' 300 800) 0
    d = modDif (mouseX KR 0 (nc_u * 2) Linear 0.2) (mce [0 .. nc_u - 1]) nc_u
in splay (x * max 0 (1 - d)) 1 1 0 True * 0.1

---- ; drawings
import Sound.SC3.Plot {- hsc3-plot -}
plot_ugen 0.01 (let a = line AR 0 4 0.01 DoNothing in mce2 a (modDif a 0 1))
plot_ugen 0.01 (let a = line AR 0 4 0.01 DoNothing in modDif a 0 (mce [1 .. 4]))
plot_ugen 0.01 (let a = line AR 0 4 0.01 DoNothing in modDif a (mce [0, 0.25 .. 1]) 1)
