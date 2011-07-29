limiter input level lookAheadTime

Peak limiter.  Limits the input amplitude to the given
level. Limiter will not overshoot like Compander will, but it needs
to look ahead in the audio. Thus there is a delay equal to twice
the lookAheadTime.  Limiter, unlike Compander, is completely
transparent for an in range signal.

 in = 0.0
 level = 1.0
 dur = 0.01
