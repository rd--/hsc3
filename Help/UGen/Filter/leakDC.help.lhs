leakDC in coef=0.995

Remove DC.  This filter removes a DC offset from a signal.  in -
input signal.  coef - leak coefficient.

> let a = lfPulse AR 800 0.5 0.5 * 0.1
> in audition (out 0 (mce [a, leakDC a 0.995]))
