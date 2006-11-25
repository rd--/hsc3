xLine start end dur doneAction

Exponential line generator.  Generates an exponential curve from the
start value to the end value. Both the start and end values must be
non-zero and have the same sign.

start      - starting value
end        - ending value
dur        - duration in seconds
doneAction - a doneAction to be evaluated when the XLine is
             completed. See EnvGen for details.

Note: The sclang interface reorders the mul and add inputs to precede
the doneAction input.

> audition $ sinOsc AR (xLine KR 200 17000 10 RemoveSynth) 0 * 0.1
