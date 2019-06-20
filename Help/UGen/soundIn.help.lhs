soundIn is a composite of the UGens in' and numOutputBuses

> import Sound.SC3 {- hsc3 -}

copy fifth second channel (index 4) to first output channel (index 0)

> gr_01 = soundIn 0

Copy input from 1 & 0 to outputs 0 & 1.

> gr_02 = soundIn (mce2 1 0)

io matrix:

  0 1 2 3
0 *
1     *
2   *
3       *

> gr_03 = soundIn (mce [0, 2, 1, 3])
