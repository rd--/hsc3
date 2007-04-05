replaceOut bufferIndex inputs

Send signal to a bus, overwrite existing signal.

> let a = out 0 (sinOsc AR (MCE [330, 331]) 0 * 0.1)
>     b = replaceOut 0 (sinOsc AR (MCE [880, 881]) 0 * 0.1)
>     c = out 0 (sinOsc AR (MCE [120, 121]) 0 * 0.1)
> audition (MRG [a, b, c])

Compare to:

> let a = out 0 (sinOsc AR (MCE [330, 331]) 0 * 0.1)
>     b = out 0 (sinOsc AR (MCE [880, 881]) 0 * 0.1)
>     c = out 0 (sinOsc AR (MCE [120, 121]) 0 * 0.1)
> audition (MRG [a, b, c])
