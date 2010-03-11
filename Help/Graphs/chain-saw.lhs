chain saw (jrhb)

[this graph generates long chains of unit generators and may require
increasing the stack limit of the haskell run time system]

> import Control.Monad
> import Sound.SC3.Monadic
> import System.Random

> main =
>    let { rrand l r = getStdRandom (randomR (l, r)) :: IO Double
>        ; coin n a b = do { m <- rrand 0.0 1.0
>                          ; return (if m > n then a else b) }
>        ; exprange s l r = linExp s (-1) 1 l r
>        ; chain n fn = foldr (<=<) return (replicate n fn)
>        ; mceProduct = mceEdit (\l -> [product l])
>        ; clipu s = clip2 s 1
>        ; dup a = mce2 a a
>        ; f s1 = do { xr <- fmap dup (expRand 0.1 2)
>                    ; n1 <- lfNoise1 kr xr
>                    ; n2 <- lfNoise1 kr xr
>                    ; n3 <- lfNoise1 kr xr
>                    ; f1 <- coin 0.6 (exprange n1 0.01 10) (exprange n2 10 50)
>                    ; s2 <- coin 0.5 (1 - s1) (mceReverse s1)
>                    ; let { f2 = linExp s1 (-1) 1 f1 (f1 * exprange n3 2 10)
>                          ; u1 = lfSaw kr f2 0
>                          ; u2 = lfSaw kr (f1 * 0.1) 0 * 0.1 + 1 }
>                   in return . clipu =<< coin 0.5 (u1 * s2) (u1 * u2) }
>        ; inp = lfSaw kr (0.2 * mce2 1 1.1) 0
>        ; b_freq = mce [70, 800, 9000, 5242] }
>   in do { ff <- chain 8 f inp
>         ; let { c_saw = mceProduct (saw ar (exprange ff 6 11000))
>               ; b_saw = dup (mix (bpf c_saw b_freq 0.2)) }
>         in audition (out 0 (b_saw * 0.3)) }

{ var f = { arg s1
          ; var rate = ExpRand.new(0.1, 2).dup
          ; var n1 = { LFNoise1.kr(rate).exprange(0.01, 10) }
          ; var n2 = { LFNoise1.kr(rate).exprange(10, 50) }
          ; var n3 = LFNoise1.kr(rate).exprange(2, 10)
          ; var f1 = if(0.6.coin) { n1.value } { n2.value }
          ; var s2 = [1 - s1, s1.reverse].choose
          ; var f2 = LinExp.kr(s1, -1, 1, f1, f1 * n3)
          ; var u1 = LFSaw.kr(f2, 0)
          ; var u2 = LFSaw.kr(f1 * 0.1, 0, 0.1, 1)
          ; var u3 = if(0.5.coin) { u1 * s2 } { u1 * u2 }
          ; u3.clip2(1) }
; var g = { arg func, n
          ; n.do { func = func <> func }
          ; func }
; var inp = LFSaw.kr(0.2 * [1, 1.1], 0)
; var b_freq = [70, 800, 9000, 5242]
; var ff = g.(f, 4).value(inp)
; var c_saw = Saw.ar(ff.exprange(6, 11000)).product
; var b_saw = BPF.ar(c_saw, b_freq, 0.2).sum.dup
; Out.ar(0, b_saw * 0.3) }.play
