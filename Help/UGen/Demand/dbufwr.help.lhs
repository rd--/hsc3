dbufwr bufnum phase input loop

Buffer demand ugen.  All inputs can be either 
demand ugen or any other ugen.

bufnum - buffer number to read from (single channel buffer)
phase  - index into the buffer
input  - single channel input
loop   - when phase exceeds number of frames in buffer, 
         loops when set to 1 (default :1)

> import Sound.SC3

> do { s1 <- dseries 30 0 3
>    ; s2 <- dseries 30 0 1
>    ; s3 <- dseries 16 1 1
>    ; s4 <- dwhite 8 1 16 
>    ; s5 <- dseq dinf (mce2 s3 s4)
>    ; wt <- dust KR 1                  {- write trigger -}
>    ; rp <- dseries dinf 0 1           {- read pointer -}
>    ; wp <- dseq dinf (mce2 s1 s2)     {- write pointer -}
>    ; r <- dbufrd 0 rp Loop            {- reader -}
>    ; w <- dbufwr 0 wp (s5 * 60) Loop  {- writer -}
>    ; let { d = demand wt 0 w
>          ; f = lag (demand (impulse KR 16 0) 0 r) 0.01
>          ; o = sinOsc AR (f * mce2 1 1.01) 0 * 0.1
>          ; g = mrg [d, out 0 o]
>          ; run fd = do { async fd (b_alloc 0 24 1)
>                        ; send fd (b_setn 0 [(0, (replicate 24 210))])
>                        ; play fd g } }
>      in withSC3 run }
