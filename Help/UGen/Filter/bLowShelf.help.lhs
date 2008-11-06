bLowShelf i f rs db

   i - input signal to be processed
   f - center frequency
  rs - the reciprocal of S.  Shelf boost/cut slope. When S = 1, the
       shelf slope is as steep as it can be and remain monotonically
       increasing or decreasing gain with frequency.  The shelf slope,
       in dB/octave, remains proportional to S for all other values
       for a fixed freq/SampleRate.ir and db.
  db - gain. boost/cut the center frequency in decibels.

> import Sound.SC3

> let { i = soundIn (mce2 0 1)
>     ; f = mouseX KR 40 6000 Exponential 0.2
>     ; rs = 1
>     ; db = mouseY KR 24 (-24) Linear 0.2 }
> in audition (out 0 (bLowShelf i f rs db))

> let { i = soundIn (mce2 0 1)
>     ; f = mouseX KR 20 6000 Exponential 0.2 
>     ; rs = mouseY KR 0.1 1 Linear 0.2 
>     ; db = 6}
> in audition (out 0 (bLowShelf i f rs db))
