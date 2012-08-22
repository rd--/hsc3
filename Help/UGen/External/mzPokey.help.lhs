> Sound.SC3.UGen.Help.viewSC3Help "MZPokey"
> Sound.SC3.UGen.DB.ugenSummary "MZPokey"

> import Sound.SC3
> import qualified Sound.SC3.Lang.Math as M

> let b = fromIntegral . M.parseBits :: (String -> UGen)
> let bln = line KR 0 255 5 RemoveSynth
> let mz1 i j = mzPokey i j 0 0 0 0 0 0 0
> let mz1c i j c = mzPokey i j 0 0 0 0 0 0 c

> audition (out 0 (mz1 bln (b "00001111")))
> audition (out 0 (mz1 bln (b "00101111")))
> audition (out 0 (mz1 bln (b "10101111")))
> audition (out 0 (mz1c bln (b "10101111") (b "00000001")))
> audition (out 0 (mz1c bln (b "10101111") (b "01000001")))

> let mz2c i j p q c = mzPokey i j p q 0 0 0 0 c
> let bX = mouseX KR 0 255 Linear 0.1
> let bY = mouseY KR 0 255 Linear 0.1

> audition (out 0 (mz2c bX (b "10101010") bY (b "10101010") (b "00000001")))

> let mz4pc (f1,c1) (f2,c2) (f3,c3) (f4,c4) c = mzPokey f1 c1 f2 c2 f3 c3 f4 c4 c

> let { v1 = (bX,b "11000111")
>     ; v2 = (bY,b "11100111")
>     ; v3 = (sinOsc KR 0.4 0 * 127.5 + 127.5,b "11000111")
>     ; v4 = (sinOsc KR 0.5 0 * 127.5 + 127.5,b "01000111")
>     ; m = mz4pc v1 v2 v3 v4 (b "00000000") }
> in audition (out 0 (mce2 m m))
