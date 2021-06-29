-- tanh
let e = xLine kr 0.1 10 10 DoNothing
    o = fSinOsc ar 500 0.0
in tanh (o * e) * 0.25

---- ; drawings
Sound.SC3.is_unary Sound.SC3.Common.Base.CS "TanH"
Sound.SC3.Plot.plot_fn_r1_ln tanh (-4,4)
