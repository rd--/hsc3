-- playBuf ; check buffer
let b = control kr "buf" 0
    s = bufRateScale kr b
in playBuf 1 ar b s 1 0 NoLoop RemoveSynth * 0.1

-- shufflerB ; static
let b = control kr "buf" 0
in X.shufflerB b 0 0.05 0.95 1.05 0.035 0.05 0.1 0.2 0.4 0.6 0.4 0.6 0 1 0.005 0.01 1 0 0

-- shufflerB ; static, quantized
let b = control kr "buf" 0
in X.shufflerB b 0 1 0.5 2 0.025 0.075 0.2 0.6 0.1 0.9 0.1 0.9 0 1 0.005 0.05 0 0.5 0.005

-- shufflerB ; static, pointilist
let b = control kr "buf" 0
in X.shufflerB b 0.4 0.5 0.5 2 0.05 0.15 0.2 0.5 0.3 0.7 0.3 0.7 0 1 0.05 0.25 0 0 0

-- shufflerB ; controls
let b = control_m kr "buf" 0 (0,10,"lin")
    k nm def = control_m kr nm def
    k2 nm (d1,d2) = control_rng kr nm (d1,d2)
    k2_mul m (c1,c2) = (c1 * m,c2 * m)
    shufflerB_grp p1 (p2,p3) (p4,p5) (p6,p7) (p8,p9) (p10,p11) (p12,p13) (p14,p15) (p16,p17) p18 p19 p20 = X.shufflerB p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20
in shufflerB_grp b
   (k2 "loc" (0.0,0.5) (0,1,"lin"))
   (k2 "incr" (0.99975,1.00025) (0.25,4,"lin"))
   (k2_mul 0.001 (k2 "dur" (250,300) (0.1,1000,"exp")))
   (k2 "amp" (0.8,0.9) (0.5,1.0,"amp"))
   (k2 "shape" (0.5,0.6) (0,1,"lin"))
   (k2 "skew" (0.4,0.6) (0,1,"lin"))
   (k2 "pan" (0.0,1.0) (0,1,"lin")) -- ie. not (-1,1)
   (k2_mul 0.001 (k2 "iot" (50,52.5) (0.01,500,"exp")))
   (k "locIncr" 1.0 (0.25,4.0,"lin"))
   (k "incrQ" 0.0 (0,1,"lin"))
   (k "iotQ" 0.0 (0,1,"lin"))

---- ; load buffer
ld fn = withSc3 (async (b_allocRead 0 (sfResolve fn) 0 0))
ld "instr/crotales/crotale05(D).wav"
