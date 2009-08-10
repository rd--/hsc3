klank in freqScale freqOffset decayScale spec

Klank is a bank of fixed frequency resonators which can be used to
simulate the resonant modes of an object. Each mode is given a ring
time, which is the time for the mode to decay by 60 dB.

The UGen assistant Klank.spec can help create the 'spec' entry.
Note that the SC3 language reorders the inputs, the Hsc client does
not.

input - the excitation input to the resonant filter bank.

freqscale - a scale factor multiplied by all frequencies at
            initialization time.

freqoffset - an offset added to all frequencies at initialization
             time.

decayscale - a scale factor multiplied by all ring times at
             initialization time.

> import Sound.SC3

> let s = klankSpec [800, 1071, 1153, 1723] [1, 1, 1, 1] [1, 1, 1, 1]
> in audition (out 0 (klank (impulse AR 2 0 * 0.1) 1 0 1 s))

There is a limited form of multiple channel expansion possible
at 'specification' input, below three equal dimensional 
specifications are tranposed and force expansion in a sensible manner.

> let { u = [1, 1, 1, 1]
>     ; p = [200, 171, 153, 172]
>     ; q = [930, 971, 953, 1323]
>     ; r = [8900, 16062, 9013, 7892]
>     ; k = mce [klankSpec p u u, klankSpec q u u, klankSpec r u u]
>     ; s = mceTranspose k
>     ; i = mce [2, 2.07, 2.13]
>     ; t = impulse AR i 0 * 0.1 }
> in audition (out 0 (mix (klank t 1 0 1 s)))
