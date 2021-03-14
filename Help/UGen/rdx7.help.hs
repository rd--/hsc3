-- rdx7 ; texture node ; data at local buffer
let vc = [[25,21,98,38,99, 0,99, 0,36,17,87,2,1,0,0,0,59,0, 1,1,8]
         ,[66,69,60,35, 0, 0,98, 0, 0, 0, 4,0,0,0,0,1,90,0, 1,0,8]
         ,[25,21,98,38,99, 0,99, 2,35,15,79,3,1,1,0,0,91,1, 0,0,6]
         ,[97,69,60,35,99,91,98, 0, 0, 0, 0,0,0,0,0,1,68,0,22,0,7]
         ,[78,63,53,28,99,52,62, 3,38,11,80,3,0,0,0,0,77,0, 7,0,0]
         ,[98,56,63,23,99,89,74, 2, 0, 4, 1,0,0,0,0,0,99,0, 1,0,7]
         ,[99,99,99,99,50,50,50,50,3,2,1,29,99,1,0,0,0,1,24]]
    dur = rand 'α' 0.1 10.0
    mnn = iRand 'β' 48 72
    vel = iRand 'γ' 10 69
    loc = rand 'δ' (-1) 1
    gate_ = line KR 1 0 dur DoNothing
    reset_ = 0
    data_ = 0
    buf = asLocalBuf 'ε' (map constant (concat vc))
    s = pan2 (X.rdx7 AR buf gate_ reset_ data_ 0 mnn vel 0x2000 0 0 0) loc 1
    d = detectSilence s 0.001 0.1 RemoveSynth
in mrg [out 0 s,d]

-- rdx7 ; event control ; data at local buffer
let f _ (g,x,_,z,o,_,_,p,_,_) =
      let vc = [[25,21,98,38,99, 0,99, 0,36,17,87,2,1,0,0,0,59,0, 1,1,8]
               ,[66,69,60,35, 0, 0,98, 0, 0, 0, 4,0,0,0,0,1,90,0, 1,0,8]
               ,[25,21,98,38,99, 0,99, 2,35,15,79,3,1,1,0,0,91,1, 0,0,6]
               ,[97,69,60,35,99,91,98, 0, 0, 0, 0,0,0,0,0,1,68,0,22,0,7]
               ,[78,63,53,28,99,52,62, 3,38,11,80,3,0,0,0,0,77,0, 7,0,0]
               ,[98,56,63,23,99,89,74, 2, 0, 4, 1,0,0,0,0,0,99,0, 1,0,7]
               ,[99,99,99,99,50,50,50,50,3,2,1,29,99,1,0,0,0,1,24]]
          buf = asLocalBuf 'ε' (map constant (concat vc))
          x0 = latch x g
      in pan2 (X.rdx7 AR buf g 0 0 0 p z (0x2000 * (x - x0)) 0 0 0) (o * 2 - 1) 1
in mix (rEventVoicer 16 f) * control KR "gain" 1

-- rdx7 ; data at shared buffer ; external control
let buf = control KR "dat" 100
    gate_ = control KR "gate" 0
    reset = control KR "reset" 0
    data_ = control KR "data" 0
    vc = control KR "vc" 0
    mnn = control KR "mnn" 60
    vel = control KR "vel" 99
    pw = 0x2000
    mw = 0
    bc = 0
    fc = 0
in X.rdx7 AR buf gate_ reset data_ vc mnn vel pw mw bc fc

-- rdx7 ; data at shared buffer
let nv = 32 -- 221
    buf = control KR "dat" 100
    tr = dust 'α' KR 2.0
    gate_ = toggleFF tr
    reset_ = 0
    data_ = 0
    vc = tRand 'β' 0 (nv - 1) tr
    mnn = tRand 'γ' 56.5 57.5 tr -- FRACTIONAL MIDI NOTE NUMBER -- 60 61
    vel = tRand 'δ' 10 29 tr
    loc = tRand 'ε' (-1) 1 tr
in pan2 (X.rdx7 AR buf gate_ reset_ data_ vc mnn vel 0x2000 0 0 0) loc 1

-- rdx7 ; event control ; data at shared buffer
let f _ (g,x,_,z,o,_,_,p,_,_) =
      let buf = control KR "buf" 100
          vc = control_md KR "vc" 0 (0,31,"lin",1,"ix")
          x0 = latch x g
          pw = 0x2000 * (1 + (x - x0) * 2)
          s = X.rdx7 AR buf g 0 0 vc p (z * 99) pw 0 0 0
      in pan2 s (o * 2 - 1) 1
in mix (rEventVoicer 16 f) * control_md KR "gain" 1 (0,4,"amp",0.01,"*")

---- ; send init voice
import qualified Sound.SC3.Data.Yamaha.DX7 as DX7 {- hsc3-data -}
import qualified Sound.SC3.Data.Yamaha.DX7.SC3 as DX7 {- hsc3-data -}
DX7.dx7_sc3_data_load 100 [DX7.dx7_init_voice]

---- ; load patch data from .hex.text file
hex_fn = "/home/rohan/uc/the-center-is-between-us/trees/text/dx7/tc.hex.text"
v <- DX7.dx7_load_hex hex_fn
DX7.dx7_sc3_data_load 100 v
length v == 221

---- ; load patch data from .syx sysex file
syx_fn = "/home/rohan/sw/hsc3-data/data/yamaha/dx7ii/rom/DX7II-32A.syx"
bnk <- DX7.dx7_load_fmt9_sysex_err syx_fn
DX7.dx7_sc3_data_load 100 bnk
length bnk == 32

---- ; send external control messages
import Sound.OSC {- hosc -}
withSC3 (sendMessage (n_set1 1 "vc" 0))
withSC3 (sendMessage (n_set1 1 "gate" 1.0))
withSC3 (sendMessage (n_set1 1 "gate" 0.0))
withSC3 (sendMessage (n_set1 1 "data" 0.0))
withSC3 (sendMessage (n_set1 1 "mnn" 69.0))
withSC3 (sendMessage (n_set1 1 "vel" 10.0))

{---- RDX7

An SC3 UGen of the
[Levien](https://github.com/google/music-synthesizer-for-android) /
[Gauthier](https://github.com/asb2m10/dexed) DX7 emulator codes.

-}
