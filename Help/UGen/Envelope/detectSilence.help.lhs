detectSilence in amp time doneAction

If the signal at `in' falls below `amp' for `time' seconds then
`doneAction' is raised.

> let s = sinOsc AR 440 0 * mouseY KR 0 0.4 Linear 0.1
> audition $ MRG [detectSilence s 0.1 0.2 RemoveSynth, out 0 s]
