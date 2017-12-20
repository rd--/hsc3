    Sound.SC3.UGen.Help.viewSC3Help "MZPokey"
    Sound.SC3.UGen.DB.ugenSummary "MZPokey"

> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Bindings.HW.External.F0 {- hsc3 -}
> import qualified Sound.SC3.Lang.Math as M

> bits_to_int :: String -> Int
> bits_to_int = M.parseBits

> bits_to_ugen :: String -> UGen
> bits_to_ugen = fromIntegral . bits_to_int

> b = bits_to_ugen
> bln = line KR 0 255 5 RemoveSynth
> mz1 i j = mzPokey i j 0 0 0 0 0 0 0
> mz1c i j c = mzPokey i j 0 0 0 0 0 0 c

> g_01 = mz1 bln (b "00001111")
> g_02 = mz1 bln (b "00101111")
> g_03 = mz1 bln (b "10101111")
> g_04 = mz1c bln (b "10101111") (b "00000001")
> g_05 = mz1c bln (b "10101111") (b "01000001")

> mz2c i j p q c = mzPokey i j p q 0 0 0 0 c
> bX = mouseX KR 0 255 Linear 0.1
> bY = mouseY KR 0 255 Linear 0.1

> g_06 = mz2c bX (b "10101010") bY (b "10101010") (b "00000001")

> mz4pc (f1,c1) (f2,c2) (f3,c3) (f4,c4) c = mzPokey f1 c1 f2 c2 f3 c3 f4 c4 c

> g_07 =
>   let v1 = (bX,b "11000111")
>       v2 = (bY,b "11100111")
>       v3 = (sinOsc KR 0.4 0 * 127.5 + 127.5,b "11000111")
>       v4 = (sinOsc KR 0.5 0 * 127.5 + 127.5,b "01000111")
>   in mz4pc v1 v2 v3 v4 (b "00000000")
