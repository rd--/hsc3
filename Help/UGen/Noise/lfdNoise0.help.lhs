> Sound.SC3.UGen.Help.viewSC3Help "LFDNoise0"
> Sound.SC3.UGen.DB.ugenSummary "LFDNoise0"

> import Sound.SC3

for fast x LFNoise frequently seems stuck, LFDNoise changes smoothly

> let x = mouseX KR 0.1 1000 Exponential 0.2
> in audition . (out 0) . (* 0.1) =<< lfdNoise0M AR x

> let x = mouseX KR 0.1 1000 Exponential 0.2
> in audition . (out 0) . (* 0.1) =<< lfNoise0M AR x

silent for 2 secs before going up in freq

> let f = xLine KR 0.5 10000 3 RemoveSynth
> in audition . (out 0) . (* 0.1) =<< lfdNoise0M AR f

> let f = xLine KR 0.5 10000 3 RemoveSynth
> in audition . (out 0) . (* 0.1) =<< lfNoise0M AR f

LFNoise quantizes time steps at high freqs, LFDNoise does not:

> let f = xLine KR 1000 20000 10 RemoveSynth
> in audition . (out 0) . (* 0.1) =<< lfdNoise0M AR f

> let f = xLine KR 1000 20000 10 RemoveSynth
> in audition . (out 0) . (* 0.1) =<< lfNoise0M AR f

Drawing

> import Sound.SC3.Plot {- hsc3-plot -}

> plot_ugen1 0.1 (lfdNoise0 'γ' AR 1000)
> plot_ugen1 0.1 (lfdNoise1 'γ' AR 1000)
> plot_ugen1 0.1 (lfdNoise3 'γ' AR 1000)
