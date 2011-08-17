detectSilence in amp=0.0001 time=0.1 doneAction=DoNothing

If the signal at `in' falls below `amp' for `time' seconds then
`doneAction' is raised.

> import Sound.SC3

> let {s = sinOsc AR 440 0 * mouseY' KR 0 0.4 Linear 0.1
>     ;d = detectSilence s 0.1 0.2 RemoveSynth}
> in audition (mrg [out 0 s,d])
