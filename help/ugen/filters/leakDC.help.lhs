leakdc in coef

Remove DC.  This filter removes a DC offset from a signal.  in -
input signal.  coef - leak coefficient.

> let a = lfpulse AR 800 0.5 0.5 0.1
> in MCE [a, leakdc AR a 0.995]
