varSaw rate freq iphasewidth

Variable duty saw

freq   - frequency in Hertz
iphase - initial phase offset in cycles ( 0..1 )
width  - duty cycle from zero to one.

> import Sound.SC3

> let { f = lfPulse KR (mce2 3 3.03) 0 0.3 * 200 + 200
>     ; w = linLin (lfTri KR 1 0) (-1) 1 0 1 }
> in audition (out 0 (varSaw AR f 0 w * 0.1))
