MZPokey.  POKEY Chip Sound Simulator

Emulation of the sound generation hardware of the POKEY chip by
Michael Borisov. This version calculates in 16bits, is bandlimited and
generally better sounding.

Code adapted from the MZPOKEY sound chip emulation, v1.6, from the
Atari800 emulator project, available here
http://atari800.sourceforge.net/ More info on how to program this chip
can be found here http://www.atariarchives.org/dere/chapt07.php and
here http://www.retroclinic.com/leopardcats/bbpokey/pokey.pdf

argument::audf1, audf2, audf3, audf4
## bits 7-0 || frequency (8bits 0-255)

argument::audc1, audc2, audc3, audc4
## bit 7 || poly5 or direct clock
## bit 6 || poly4 or poly17
## bit 5 || poly4/17 or pure tone
## bit 4 || volume output only
## bits 3-0 || volume (4bits 0-15)

argument::audctl
## bit 7 || switches main clock base from 64KHz to 15KHz
## bit 6 || inserts high-pass filter into channel 2, clocked by channel 4
## bit 5 || inserts high-pass filter into channel 1, clocked by channel 3
## bit 4 || joins channel 4 to channel 3 (16bit resolution)
## bit 3 || joins channel 2 to channel 1 (16bit resolution)
## bit 2 || clocks channel 3 with 1.79MHz
## bit 1 || clocks channel 1 with 1.79MHz
## bit 0 || makes the 17bit poly-counter into a 9bit poly-counter

> import Sound.SC3
> import Sound.SC3.Lang.Math.Binary

> let b = fromIntegral . parseBits :: (String -> UGen)
> let bln = line KR 0 255 5 RemoveSynth
> let mz1 i j = mzPokey i j 0 0 0 0 0 0 0
> let mz1c i j c = mzPokey i j 0 0 0 0 0 0 c

> audition (out 0 (mz1 bln (b "00001111")))
> audition (out 0 (mz1 bln (b "00101111")))
> audition (out 0 (mz1 bln (b "10101111")))
> audition (out 0 (mz1c bln (b "10101111") (b "00000001")))
> audition (out 0 (mz1c bln (b "10101111") (b "01000001")))

> let mz2c i j p q c = mzPokey i j p q 0 0 0 0 c
> let bX = mouseX' KR 0 255 Linear 0.1
> let bY = mouseY' KR 0 255 Linear 0.1

> audition (out 0 (mz2c bX (b "10101010") bY (b "10101010") (b "00000001")))

> let mz4pc (f1,c1) (f2,c2) (f3,c3) (f4,c4) c = mzPokey f1 c1 f2 c2 f3 c3 f4 c4 c

> let { v1 = (bX,b "11000111")
>     ; v2 = (bY,b "11100111")
>     ; v3 = (sinOsc KR 0.4 0 * 127.5 + 127.5,b "11000111")
>     ; v4 = (sinOsc KR 0.5 0 * 127.5 + 127.5,b "01000111")
>     ; m = mz4pc v1 v2 v3 v4 (b "00000000") }
> in audition (out 0 (mce2 m m))
