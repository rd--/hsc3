{-
Alvin Lucier, "Music on a Long Thin Wire, Simulated"
Chandrasekhar Ramakrishnan
http://www.listarc.bham.ac.uk/lists/sc-users/msg47539.html
http://www.listarc.bham.ac.uk/lists/sc-users/msg47540.html
-}

import Sound.SC3.ID

lucier_wire :: UGen -> IO ()
lucier_wire freq =
    let block_size = recip controlRate
        mk_dt f = recip f - block_size
	string_delay = mk_dt freq
	pk1_pos = 0.1 -- pickup one position
	src_pos = 0.3 -- source position
	pk2_pos = 0.9 -- pickup two postion
	max_delay = 1.0 -- maximum delay time (corresponds to a length = c * s)
	mk_delay i r = lpz1 (delayC i max_delay (r * string_delay))
	mk_allpass i r dt = lpz1 (allpassC i max_delay (r * string_delay) dt)
	drv = localIn 1 AR -- driver (source + data stored in the string)
	pk1_R =
            let i = drv
                r = src_pos - pk1_pos
            in mk_delay i r
	pk1_L =
            let i = pk1_R * negate 1
                r = pk1_pos * 2
                dt = rand 'α' 0.001 0.11
            in mk_allpass i r dt
	pk2_L =
            let i = pk1_L
                r = pk2_pos - pk1_pos
            in mk_delay i r * 0.99
	stringL =
            let i = pk2_L
                r = 1.0 - pk2_pos
            in mk_delay i r
	pk2_R =
            let i = stringL * negate 1
                r = 1.0 - pk2_pos
                dt = 2 + rand 'β' 0.001 0.11
            in mk_allpass i r dt * 0.99
	stringR =
            let i = pk2_R
                r = pk2_pos - src_pos
            in mk_delay i r
	source =
            let s = sinOsc AR 220 0 * 0.01
                p = pulse AR (60 + amplitude KR drv 0.01 0.01 * 11) 0.5 * 0.1
                f = rlpf (s + p) 320 0.05
                e = 1.0 - min (amplitude KR drv 0.01 0.01) 1.0
	    in normalizer f 0.7 0.01 * e
	l_out = localOut (source * 0.2 + stringR)
	outL = pk1_L + pk1_R
	outR = pk2_L + pk2_R
    in audition (out 0 (mrg [mce2 outL outR, drv, source, l_out]))

main = lucier_wire 60

{-
lucier_wire 60

{ var blocksize = ControlRate.ir.reciprocal
; var mk_dt = { |f| f.reciprocal - blocksize }
; var string_dt = mk_dt.(60)
; var c = 425
; var len = 40
; var pk1_p = 0.1
; var src_p = 0.3
; var pk2_p = 0.9
; var max_dt = 1
; var mk_dl = { arg i, r
                 ; LPZ1.ar(DelayC.ar(i, max_dt, r * string_dt)) }
; var mk_ap = { arg i, r, dt
	          ; LPZ1.ar(AllpassC.ar(i, max_dt, r * string_dt, dt)) }
; var drv = LocalIn.ar(1)
; var r = { Rand(0.001, 0.11) }
; var pk1_R = mk_dl.(drv, src_p - pk1_p, 2 + r.())
; var pk1_L = mk_ap.(pk1_R * -1, pk1_p * 2, r.())
; var pk2_L = mk_dl.(pk1_L, pk2_p - pk1_p, r.()) * 0.99
; var strL = mk_dl.(pk2_L, 1 - pk2_p, r.())
; var pk2_R = mk_ap.(strL * -1, 1 - pk2_p, 2 + r.()) * 0.99
; var strR = mk_dl.(pk2_R, pk2_p - src_p, 2 + r.())
; var src = { var a = Amplitude.kr(drv, mul: 11)
            ; p = Pulse.ar(60 + a, mul: 0.1)
            ; RLPF.ar((SinOsc.ar(220, 0) * 0.01) + p, 320, 0.05) }
; var src_n = { var a = Amplitude.kr(drv).min(1.0)
              ; Normalizer.ar(src, 0.7) * (1.0 - a) }
; LocalOut.ar(src * 0.2 + strR)
; [pk1_L + pk1_R, pk2_L + pk2_R, drv, src] }.play

-}
