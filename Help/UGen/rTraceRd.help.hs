-- rTraceRd ; printing only
let b = asLocalBuf 'α' [0,-1,1/4, 1/4,0,1/2, 1/2,1,1/4, 3/4,0,0, 1,-1,1/4]
    ph = mouseX kr 0 1 Linear 0.05
    tr = X.rTraceRd kr b 3 ph 1
    labels = mce . map label
    pr = poll (impulse kr 5 0) (mce2 ph tr) 0 (labels ["ph","tr"])
in mrg2 (out 0 (dc ar 0)) pr

-- rTraceRd ; listening ; X is (-1 0 1 0 -1)
let b = asLocalBuf 'α' [0,-1,1/4, 1/4,0,1/2, 1/2,1,1/4, 3/4,0,0, 1,-1,1/4]
    o = sinOsc ar 440 0 * 0.1
    x = X.rTraceRd kr b 3 (lfSaw kr 1 0 * 0.5 + 0.5) 1
in pan2 o x 1

-- rTraceRd
let b = asLocalBuf 'α' [0,-1,1/4, 1/4,0,1/2, 1/2,1,1/4, 3/4,0,0, 1,-1,1/4]
    x = X.rTraceRd kr b 3 (lfSaw kr 1 0 * 0.5 + 0.5) 1
    o = sinOsc ar (linLin x (-1) 1 440 880) 0 * 0.1
in pan2 o 0 1

-- rTraceRd ; Y is (1/4 1/2 1/4 0 1/4)
let b = asLocalBuf 'α' [0,-1,1/4, 1/4,0,1/2, 1/2,1,1/4, 3/4,0,0, 1,-1,1/4]
    o = sinOsc ar 440 0 * 0.1
    tr n = X.rTraceRd kr b 3 (lfSaw kr 1 0 * 0.5 + 0.5) n
    l = tr 1
    g = tr 2
in pan2 o l g

---- ; loading data from a CSV trace file
import qualified Sound.SC3.Data.Trace as T {- hsc3-data -}
let fn = "/home/rohan/sw/hsc3-data/data/csv/trace/b.csv"
tr <- T.trace_load_csv2 fn
length tr == 3017
let tr' = T.trace_normalise_t tr
let d = concatMap (\(t,(x,y)) -> [t,x,y]) tr'
length d == length tr * 3
withSC3 (async (b_alloc_setn1 0 0 d) >> return ())

{----

UGens to read a buffer that is arranged as a trace.  A trace is a
sequence of tuples (T,X...) where the number of data points (including
T) is given by the 'degree' input at the UGen.

-}
