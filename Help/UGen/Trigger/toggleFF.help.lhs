toggleFF trig

Toggle flip flop. Toggles between zero and one upon receiving a trigger.

trig - trigger input

> t <- dust AR (xLine KR 1 1000 60 DoNothing)
> audition $ sinOsc AR (toggleFF t * 400 + 800) 0 * 0.1
