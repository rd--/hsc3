tsort (jmcc)

This simple graph tests the topological sort of the unit generator
graph, it ought only to use a minimal number of interconnect buffers.

The below 369 node graph works with 'scsynth -u 57110 -w 2'.  

(Note that graphs loaded from disk during startup will grow the number
of interconnect buffers, so to test this we must delete all graphs that
would otherwise be loaded.)

> import Sound.SC3

> main =
>   let { n = 122
>       ; c = envCoord [(0,0), (0.15, 1), (6, 0)] 1 1 EnvLin
>       ; e = envGen kr 1 1 0 1 RemoveSynth c
>       ; f o = sinOsc ar (440 + constant o) 0 * 0.001
>       ; s = mixFill n f }
>   in audition (out 0 (s * e))

http://create.ucsb.edu/pipermail/sc-users/2003-March/002807.html
