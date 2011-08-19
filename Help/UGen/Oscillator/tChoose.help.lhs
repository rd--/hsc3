tChoose trig inputs

The output is selected randomly on recieving a trigger from an
array of inputs.  tChoose is a composite of tiRand and select.

> import Sound.SC3.ID

> let { x = mouseX' kr 1 1000 Exponential 0.1
>     ; t = dust 'a' AR x
>     ; f = midiCPS (tiRand 'b' 48 60 t)
>     ; o = let a = mce [ sinOsc AR f 0
>                       , saw AR (f * 2)
>                       , pulse AR (f * 0.5) 0.1 ]
>           in tChoose 'c' t a }
> in audition (out 0 (o * 0.1))

;; Note: all the ugens are continously running. This may not be the
;; most efficient way if each input is cpu-expensive.
