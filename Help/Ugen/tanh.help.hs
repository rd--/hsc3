-- tanh
let e = xLine kr 0.1 10 10 DoNothing
    o = fSinOsc ar 500 0.0
in tanh (o * e) * 0.25

---- ; drawings
Sound.Sc3.is_unary Sound.Sc3.Common.Base.CS "TanH"
Sound.Sc3.Plot.plot_fn_r1_ln tanh (-4,4)
