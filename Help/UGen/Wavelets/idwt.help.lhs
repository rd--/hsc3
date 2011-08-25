idwt buffer wintype=0 winsize=0 wavelettype=0

The wavelet transform analyzes the input signal with respect to a
mother wavelet at a number of scales. The IDWT UGen returns from the
wavelet coefficient domain to the time domain.

Most often this is used as the end of a process which begins with DWT,
followed by scale-domain processing using WT (wavelet-transform)
UGens, followed by IDWT.

buffer - The dwt chain signal coming originally from an DWT UGen,
perhaps via other WT UGens.

wintype - defines how the data is windowed: -1 is for rectangular
windowing, simple but typically not recommended; 0 (the default) is
for Sine windowing, typically recommended for work when the signal is
reconstructed; 1 is for Hann windowing, typically recommended for
analysis work.

winsize - can be used to account for zero-padding, in the same way as
the DWT UGen.

wavelettype - The choices of wavelet are those available in the gsl
wavelet library code (0-41)

The IDWT UGen converts the DWT data in-place (in the original DWT
buffer) and overlap-adds the result to produce a continuous signal at
its output.

> import Sound.SC3.ID

> let {i = whiteNoise 'a' AR * 0.05
>     ;b = mrg2 (localBuf 'α' 1024 1) (maxLocalBufs 1)
>     ;c = dwt b i 0.5 0 1 0 0}
> in audition (out 0 (mce2 (idwt c 0 0 0) i))

direct synthesis via writing values to buffer (try changing wavelet
type...)

> withSC3 (\fd -> do {_ <- async fd (b_alloc 10 1024 1)
>                    ;send fd (b_zero 10)})

> let {c = fftTrigger 10 0.5 0
>     ;i = idwt c (-1) 0 0}
> in audition (out 0 (i * 0.1))

run this to change sound: WARNING, NOISY!

> import Control.Monad.Random
> import Sound.SC3.Lang.Math.Random

> withSC3 (\fd -> send fd (b_zero 10))

> do {a <- evalRandIO (nrrand 1024 (-1) 1)
>    ;withSC3 (\fd -> send fd (b_setn 10 [(0,a)]))}

> let a = map (/ 1024) [0..1023]
> in withSC3 (\fd -> send fd (b_setn 10 [(0,a)]))

> let a = map (\i -> 1 - i / 1024) [0..1023]
> in withSC3 (\fd -> send fd (b_setn 10 [(0,a)]))
