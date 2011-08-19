bHiShelf i f rs db

   i - input signal to be processed
   f - center frequency
  rs - the reciprocal of S.  Shell boost/cut slope. When S = 1, the
       shelf slope is as steep as it can be and remain monotonically
       increasing or decreasing gain with frequency.  The shelf slope,
       in dB/octave, remains proportional to S for all other values
       for a fixed freq/SampleRate.ir and db.
  db - gain. boost/cut the center frequency in decibels.

> import Sound.SC3

> let { i = soundIn (mce2 0 1)
>     ; f = mouseX' KR 2200 18000 Exponential 0.2 
>     ; db = mouseY' KR 18 (-18) Linear 0.2 }
> in audition (out 0 (bHiShelf i f 1 db))

> let { i = soundIn (mce2 0 1)
>     ; f = mouseX' KR 2200 18000 Exponential 0.2 
>     ; rs = mouseY' KR 0.1 1 Linear 0.2 }
> in audition (out 0 (bHiShelf i f rs 6))
