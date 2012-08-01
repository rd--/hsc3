> Sound.SC3.UGen.Help.viewSC3Help "LagIn"
> Sound.SC3.UGen.DB.ugenSummary "LagIn"

> import Sound.SC3

Set frequency at control bus
> withSC3 (send (c_set1 10 200))

Oscillator reading frequency at control bus
> audition (out 0 (sinOsc AR (lagIn 1 10 1) 0 * 0.1))

Re-set frequency at control bus
> withSC3 (send (c_set1 10 2000))
