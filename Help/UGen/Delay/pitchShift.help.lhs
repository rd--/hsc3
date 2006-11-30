pitchShift in winSize pchRatio pchDispersion timeDispersion

A simple time domain pitch shifter.

> let r = mouseX KR 0.5 2 Linear 0.2
>     d = mouseY KR 0 0.1 Exponential 0.2
> audition $ pitchShift AR (sinOsc AR 440 0) 0.2 r d 0.0
