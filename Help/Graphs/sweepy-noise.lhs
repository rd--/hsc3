sweepy noise (jmcc)

> do { n <- clone 2 (whiteNoise AR)
>    ; let { lfoDepth = mouseY KR 200 8000 Exponential 0.1
>          ; lfoRate = mouseX KR 4 60 Exponential 0.1
>          ; freq = lfSaw KR lfoRate 0 * lfoDepth + (lfoDepth * 1.2)
>          ; filtered = rlpf (n * 0.03) freq 0.1 }
>      in audition (out 0 (combN filtered 0.3 0.3 2 + filtered)) }

{ var lfoDepth = MouseY.kr(200, 8000, 'exponential');
; var lfoRate = MouseX.kr(4, 60, 'exponential');
; var freq = LFSaw.kr(lfoRate, 0, lfoDepth, lfoDepth * 1.2);
; var filtered = RLPF.ar(WhiteNoise.ar([0.03,0.03]), freq, 0.1);
; Out.ar(0, CombN.ar(filtered, 0.3, 0.3, 2, 1, filtered)) }.play
