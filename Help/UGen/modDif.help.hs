-- modDif
fSinOsc ar 440 0 * (modDif 0.2 (fSinOsc ar 2 0 * 0.5) 1)

-- modDif ; different moduli
let sig = lfSaw ar 10 0
    dist = modDif sig 0 (mce [0..8] * mouseX kr 0 (1/5) Linear 0.2)
in splay (sinOsc ar (dist * 4000 + 400) 0) 1 1 0 True * 0.1

-- modDif ; wrapping amplitude crossfade
let nc = 12
    nc_u = constant nc
    x = sinOsc ar (X.rRandNId nc 'α' 300 800) 0
    d = modDif (mouseX kr 0 (nc_u * 2) Linear 0.2) (mce [0 .. nc_u - 1]) nc_u
in splay (x * max 0 (1 - d)) 1 1 0 True * 0.1

---- ; drawings
import Sound.SC3.Plot {- hsc3-plot -}
plot_ugen 0.01 (let a = line ar 0 4 0.01 DoNothing in mce2 a (modDif a 0 1))
plot_ugen 0.01 (let a = line ar 0 4 0.01 DoNothing in modDif a 0 (mce [1 .. 4]))
plot_ugen 0.01 (let a = line ar 0 4 0.01 DoNothing in modDif a (mce [0, 0.25 .. 1]) 1)
