-- excess
(fSinOsc ar 1000 0 `excess` line kr 0 1 8 DoNothing) * 0.1

-- excess ; written out in terms of clip2
let o = fSinOsc ar 1000 0
    l = line kr 0 1 8 DoNothing
in (o - clip2 o l) * 0.1

---- ; drawings
Sound.Sc3.Plot.plot_fn_r1_ln (\x -> excess x 1) (-2,2)
