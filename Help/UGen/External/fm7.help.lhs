fm7 ctl mod

  fm7, stefan kersten, phase modulation oscillator matrix.
  http://darcs.k-hornz.de/repos/skUG/

fm7 implements a 6x6 oscillator matrix, where each
oscillator's phase can be modulated by any of the
other oscillators' output.

The UGen expects two (flattened) matrices: one for
specifying the oscillator parameters frequency
(control rate), phase (initialization only) and
amplitude (control rate):

[ [ 300, 0,    1   ],
  [ 400, pi/2, 1   ],
  [ 730, 0,    0.5 ],
  [ 0,   0,    0   ],
  [ 0,   0,    0   ],
  [ 0,   0,    0   ] ]

The modulation matrix specifies the amount of
modulation each oscillator output has on another
oscillator's phase. Row i in the matrix refer to
oscillator i's phase input and the columns denote
the amount of phase modulation in radians.

The UGen outputs the six individual oscillator
signals.

> import Sound.SC3

> let { c = [[xLine KR 300 310 4 DoNothing,0,1]
>           ,[xLine KR 300 310 8 DoNothing,0,1]
>           ,[0,0,1]
>           ,[0,0,1]
>           ,[0,0,1]
>           ,[0,0,1] ]
>     ; m = [[line KR 0 0.001 2 DoNothing,line KR 0.1 0 4 DoNothing,0,0,0,0]
>           ,[line KR 0 6 1 DoNothing,0,0,0,0,0]
>           ,[0,0,0,0,0,0]
>           ,[0,0,0,0,0,0]
>           ,[0,0,0,0,0,0]
>           ,[0,0,0,0,0,0] ]
>     ; MCE [l,r,_,_,_,_] = fm7 c m }
> in audition (out 0 (mce2 l r * 0.1))

An algorithmically generated graph courtesy f0.
> let { x = [[[0.0,-1/3,-1.0,0.0]
>            ,[0.75,0.75,0.0,-0.5]
>            ,[-0.5,-0.25,0.25,-0.75]
>            ,[-0.5,1.0,1.0,1.0]
>            ,[0.0,1/6,-0.75,-1.0]
>            ,[0.5,0.5,-0.5,1/3]]
>           ,[[-1/3,0.5,-0.5,-0.5]
>            ,[0.5,0.75,0.25,0.75]
>            ,[-15/18,0.25,-1.0,0.5]
>            ,[1.5,0.25,0.25,-0.25]
>            ,[-2/3,-2/3,-1.0,-0.5]
>            ,[-1.0,0.0,-15/18,-1/3]]
>           ,[[0.25,-0.5,-0.5,-1.0]
>            ,[-0.5,1.0,-1.5,0.0]
>            ,[-1.0,-1.5,-0.5,0.0]
>            ,[0.5,-1.0,7/6,-0.5]
>            ,[15/18,-0.75,-1.5,0.5]
>            ,[0.25,-1.0,0.5,1.0]]
>           ,[[1.0,1/3,0.0,-0.75]
>            ,[-0.25,0.0,0.0,-0.5]
>            ,[-0.5,-0.5,0.0,0.5]
>            ,[1.0,0.75,0.5,0.5]
>            ,[0.0,1.5,-0.5,0.0]
>            ,[1.0,0.0,-0.25,-0.5]]
>           ,[[0.5,-0.25,0.0,1/3]
>            ,[0.25,-0.75,1/3,-1.0]
>            ,[-0.25,-0.5,0.25,-7/6]
>            ,[0.0,0.25,0.5,1/6]
>            ,[-1.0,-0.5,15/18,-0.5]
>            ,[15/18,-0.75,-0.5,0.0]]
>           ,[[0.0,-0.75,-1/6,0.0]
>            ,[1.0,0.5,0.5,0.0]
>            ,[-0.5,0.0,-0.5,0.0]
>            ,[-0.5,-1/6,0.0,0.5]
>            ,[-0.25,1/6,-0.75,0.25]
>            ,[-7/6,-4/3,-1/6,1.5]]]
>     ; y = [[[0.0,-0.5,1.0,0.0]
>            ,[-0.5,1.0,0.5,-0.5]
>            ,[0.0,1/3,1.0,1.0]]
>           ,[[-0.5,0.5,1.0,1.0]
>            ,[0.0,1/3,0.0,1.5]
>            ,[-0.5,15/18,1.0,0.0]]
>           ,[[0.25,-2/3,0.25,0.0]
>            ,[0.5,-0.5,-0.5,-0.5]
>            ,[0.5,-0.5,-0.75,15/18]]
>           ,[[-0.25,1.0,0.0,1/3]
>            ,[-1.25,-0.25,0.5,0.0]
>            ,[0.0,-1.25,-0.25,-0.5]]
>           ,[[0.75,-0.25,1.5,0.0]
>            ,[0.25,-1.5,0.5,0.5]
>            ,[-0.5,-0.5,-0.5,-0.25]]
>           ,[[0.0,0.5,-0.5,0.25]
>            ,[0.25,0.5,-1/3,0.0]
>            ,[1.0,0.5,-1/6,0.5]]]
>     ; cs = map (map (\[f,p,m,a] -> sinOsc AR f p * m + a)) x
>     ; ms = map (map (\[f,w,m,a] -> pulse AR f w * m + a)) y
>     ; MCE [c1,c2,c3,_,c4,c5] = fm7 cs ms
>     ; g3 = linLin (lfSaw KR 0.1 0) (-1) 1 0 (dbAmp (-12))
>     ; g5 = dbAmp (-3) }
> in audition (out 0 (mce [c1 + c3 * g3 + c5 * g5,c2 + c4 + c5 * g5]))
