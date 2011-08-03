metro rate bpm numBeats

Metronome. Outputs a single sample impulse according to bpm and numBeats

      bpm - beats per minute
 numBeats - number of beats before the next pulse

bpm and numBeats are polled on every impulse of output. These can be
scalers, audio rate, control rate or Demand UGens

> import Sound.SC3.ID

> audition (out 0 (metro AR 60 1))

> let { b = xLine KR 60 120 5 DoNothing
>     ; m = metro KR b 1
>     ; o = sinOsc AR 440 0 * 0.1 }
> in audition (out 0 (decay m 0.2 * o))

> let { b = range 30 240 (lfNoise2 'a' KR 0.2)
>     ; n = dseq 'b' dinf (mce [1,0.25,0.5,0.25])
>     ; a = decay (metro KR b n) 0.2 * sinOsc AR 440 0 * 0.1 }
> in audition (out 0 a)
