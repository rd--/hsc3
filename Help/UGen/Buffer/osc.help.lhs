> Sound.SC3.UGen.Help.viewSC3Help "Osc"
> Sound.SC3.UGen.DB.ugenSummary "Osc"

> import Sound.SC3

Allocate and generate wavetable buffer
> withSC3 (\fd -> do {_ <- async fd (b_alloc 10 512 1)
>                    ;let f = [Normalise,Wavetable,Clear]
>                     in send fd (b_gen_sine1 10 f [1,1/2,1/3,1/4,1/5])})

Fixed frequency wavetable oscillator
> audition (out 0 (osc AR 10 220 0 * 0.1))

Modulate frequency
> let f = xLine KR 2000 200 1 DoNothing
> in audition (out 0 (osc AR 10 f 0 * 0.1))

As frequency modulator
> let f = osc AR 10 (xLine KR 1 1000 9 RemoveSynth) 0 * 200 + 800
> in audition (out 0 (osc AR 10 f 0 * 0.1))

As phase modulatulator
> let p = osc AR 10 (xLine KR 20 8000 10 RemoveSynth) 0 * 2 * pi
> in audition (out 0 (osc AR 10 800 p * 0.1))

Fixed frequency wavetable oscillator
> audition (out 0 (osc AR 10 220 0 * 0.1))

Change the wavetable while its playing
> let f = [Normalise,Wavetable,Clear]
> in withSC3 (\fd -> send fd (b_gen_sine1 10 f [1,0.6,1/4]))

Send directly calculated wavetable
> import Sound.SC3.Lang.Collection {- hsc3-lang -}
> import Sound.SC3.Lang.Math.Window
> let t = to_wavetable (triangular_table 512)
> withSC3 (\fd -> send fd (b_setn1 10 0 t))
