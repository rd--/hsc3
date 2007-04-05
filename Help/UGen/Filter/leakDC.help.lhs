leakDC in coef

Remove DC.  This filter removes a DC offset from a signal.  in -
input signal.  coef - leak coefficient.

> let a = lfPulse AR 800 0.5 0.5 * 0.1
> audition (out 0 (MCE [a, leakDC a 0.995]))
