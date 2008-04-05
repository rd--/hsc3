stkFlute rate freq jetDelay noisegain vibFreq vibGain breathPressure tr

> let { bp = line KR 76 32 3 RemoveSynth
>     ; ng = line KR 16 64 3 DoNothing }
> in audition (out 0 (stkFlute AR 400 64 ng 16 16 bp 1))
