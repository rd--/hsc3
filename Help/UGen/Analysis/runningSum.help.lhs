runningSum in numSamp

A running sum over a user specified number of samples, useful for
running RMS power windowing.

in      - Input signal
numsamp - How many samples to take the running sum over 
          (initialisation rate)

> let a = runningSum (in' 1 AR numOutputBuses) 40 * (1/40)
> audition (out 0 (sinOsc AR 440 0 * a))
