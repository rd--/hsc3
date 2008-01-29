chain saw

> let { coin = getStdRandom random
>     ; choose a b = do { c <- coin ; return (if c then a else b) }
>     ; expRange s l r = linExp s 0 1 l r
>     ; chain n fn = foldl (>=>) return (replicate n fn)
>     ; mceProduct = mceEdit (\l -> [product l])
>     ; f s = do { r1 <- clone 2 (expRand 0.1 2)
>                ; r2 <- clone 2 (expRand 0.1 2)
>                ; n1 <- lfNoise1 KR r1
>                ; n2 <- lfNoise1 KR r2
>                ; c1 <- coin
>                ; c2 <- coin
>                ; s_ <- choose (1 - s) (mceReverse s)
>                ; let { f1 = if c1
>                             then expRange n1 0.01 10 
>                             else expRange n1 10 50
>                      ; f2 = linExp s (-1) 1 f1 (f1 * expRange n2 2 10)
>                      ; u1 = lfSaw KR f2 0
>                      ; u2 = if c2
>                             then u1 * s_
>                             else u1 * lfSaw KR (f1 * 0.1) 0 * 0.1 + 1 }
>                  in return (clip2 u2 1) }
>     ; inp = lfSaw KR (0.2 * mce2 1 1.1) 0
>     ; b_freq = mce [70, 800, 9000, 5242] }
> in do { ff <- chain 4 f inp
>       ; let { c_saw = mceProduct (saw AR (expRange ff 6 11000))
>             ; b_saw = mix (bpf c_saw b_freq 0.2) }
>         in audition (out 0 (pan2 b_saw 0 0.6)) }

{ var f = { arg saw
          ; var rate = ExpRand(0.1, 2)
          ; var a_l = { LFNoise1.kr(rate.dup).exprange(0.01, 10) }
          ; var a_r = { LFNoise1.kr(rate.dup).exprange(10, 50) }
          ; var freq = if(0.6.coin) { a_l.value } { a_r.value }
          ; var n = LFNoise1.kr(rate.dup).exprange(2, 10)
          ; var u = LFSaw.kr(LinExp.kr(saw, -1, 1, freq, freq * n), 0)
          ; var b_l = { u * [1 - saw, saw.reverse].choose }
          ; var b_r = { u * LFSaw.kr(freq * 0.1, 0, 0.1, 1) }
          ; u = if(0.5.coin) { b_l.value } { b_r.value }
          ; u.clip2(1.0) }
; var g = { arg func, n
          ; n.do { func = func <> func }
          ; func }
; var inp = LFSaw.kr(0.2 * [1, 1.1], 0)
; var freq = g.(f, 4).value(inp).exprange(6, 11000)
; var b_freq = [70, 800, 9000, 5242]
; var c_saw = Saw.ar(freq).product
; var b_saw = BPF.ar(c_saw, b_freq, 0.2).sum.dup
; Out.ar(0, b_saw * 0.3) }.play
