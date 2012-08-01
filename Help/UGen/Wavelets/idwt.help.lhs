> Sound.SC3.UGen.Help.viewSC3Help "IDWT"
> Sound.SC3.UGen.DB.ugenSummary "IDWT"

> import Sound.SC3.ID

> let {i = whiteNoise 'a' AR * 0.05
>     ;b = mrg2 (localBuf 'α' 1024 1) (maxLocalBufs 1)
>     ;c = dwt b i 0.5 0 1 0 0}
> in audition (out 0 (mce2 (idwt c 0 0 0) i))

direct synthesis via writing values to buffer (try changing wavelet
type...)
> withSC3 (async (b_alloc 10 1024 1) >> send (b_zero 10))

> let {c = fftTrigger 10 0.5 0
>     ;i = idwt c (-1) 0 0}
> in audition (out 0 (i * 0.1))

> import Control.Monad.Random
> import Sound.SC3.Lang.Random.Monad

> withSC3 (send (b_zero 10))

run this to change sound: WARNING, NOISY!
> do {a <- evalRandIO (nrrand 1024 (-1) 1)
>    ;withSC3 (send (b_setn 10 [(0,a)]))}

> let a = map (/ 1024) [0..1023]
> in withSC3 (send (b_setn 10 [(0,a)]))

> let a = map (\i -> 1 - i / 1024) [0..1023]
> in withSC3 (send (b_setn 10 [(0,a)]))
