bBandStop i f bw


   i - input signal to be processed
   f - center frequency
  bw - the bandwidth in octaves between -3 dB frequencies

> import Sound.SC3

> let { i = soundIn (mce2 0 1)
>     ; f = mouseX' KR 20 20000 Exponential 0.2 
>     ; bw = mouseY' KR 0 10 Linear 0.2 }
> in audition (out 0 (bBandStop i f bw))
