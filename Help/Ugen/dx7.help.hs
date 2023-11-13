-- dx7 ; texture node ; data at local buffer
let vc = [[25,21,98,38,99, 0,99, 0,36,17,87,2,1,0,0,0,59,0, 1,1,8]
         ,[66,69,60,35, 0, 0,98, 0, 0, 0, 4,0,0,0,0,1,90,0, 1,0,8]
         ,[25,21,98,38,99, 0,99, 2,35,15,79,3,1,1,0,0,91,1, 0,0,6]
         ,[97,69,60,35,99,91,98, 0, 0, 0, 0,0,0,0,0,1,68,0,22,0,7]
         ,[78,63,53,28,99,52,62, 3,38,11,80,3,0,0,0,0,77,0, 7,0,0]
         ,[98,56,63,23,99,89,74, 2, 0, 4, 1,0,0,0,0,0,99,0, 1,0,7]
         ,[99,99,99,99,50,50,50,50,3,2,1,29,99,1,0,0,0,1,24]]
    dur = randId 'α' 0.1 10.0
    mnn = iRandId 'β' 48 72
    vel = iRandId 'γ' 10 69
    loc = randId 'δ' (-1) 1
    gate_ = line kr 1 0 dur DoNothing
    reset_ = 0
    data_ = 0
    buf = asLocalBufId 'ε' (map constant (concat vc))
    s = pan2 (X.dx7 ar buf gate_ reset_ data_ 0 mnn vel 0x2000 0 0 0) loc 1
    d = detectSilence s 0.001 0.1 RemoveSynth
in mrg [out 0 s,d]

-- dx7 ; event control ; data at local buffer
let f (_,g,x,_,z,o,_,_,p,_,_) =
      let vc = [[25,21,98,38,99, 0,99, 0,36,17,87,2,1,0,0,0,59,0, 1,1,8]
               ,[66,69,60,35, 0, 0,98, 0, 0, 0, 4,0,0,0,0,1,90,0, 1,0,8]
               ,[25,21,98,38,99, 0,99, 2,35,15,79,3,1,1,0,0,91,1, 0,0,6]
               ,[97,69,60,35,99,91,98, 0, 0, 0, 0,0,0,0,0,1,68,0,22,0,7]
               ,[78,63,53,28,99,52,62, 3,38,11,80,3,0,0,0,0,77,0, 7,0,0]
               ,[98,56,63,23,99,89,74, 2, 0, 4, 1,0,0,0,0,0,99,0, 1,0,7]
               ,[99,99,99,99,50,50,50,50,3,2,1,29,99,1,0,0,0,1,24]]
          buf = asLocalBufId 'ε' (map constant (concat vc))
          x0 = latch x g
      in pan2 (X.dx7 ar buf g 0 0 0 (p * 100) z (0x2000 * (x - x0)) 0 0 0) (o * 2 - 1) 1
in mix (voicer 16 f) * control kr "gain" 1

-- dx7 ; data at shared buffer ; external control
let buf = control kr "dat" 400
    gate_ = control kr "gate" 0
    reset = control kr "reset" 0
    data_ = control kr "data" 0
    vc = control kr "vc" 0
    mnn = control kr "mnn" 60
    vel = control kr "vel" 99
    pw = 0x2000
    mw = 0
    bc = 0
    fc = 0
in X.dx7 ar buf gate_ reset data_ vc mnn vel pw mw bc fc

-- dx7 ; data at shared buffer
let nv = 32 -- 221
    buf = control kr "dat" 400
    tr = dustId 'α' kr 2.0
    gate_ = toggleFF tr
    reset_ = 0
    data_ = 0
    vc = tRandId 'β' 0 (nv - 1) tr
    mnn = tRandId 'γ' 56.5 57.5 tr -- fractional midi note number -- 60 61
    vel = tRandId 'δ' 10 29 tr
    loc = tRandId 'ε' (-1) 1 tr
in pan2 (X.dx7 ar buf gate_ reset_ data_ vc mnn vel 0x2000 0 0 0) loc 1

-- dx7 ; event control ; data at shared buffer
let f (_,g,x,_,z,o,_,_,p,_,_) =
      let buf = control kr "buf" 400
          vc = control_m kr "vc" 0 (0,31,"lin")
          x0 = latch x g
          pw = 0x2000 * (1 + (x - x0) * 2)
          s = X.dx7 ar buf g 0 0 vc (p * 127) (z * 99) pw 0 0 0
      in pan2 s (o * 2 - 1) 1
in mix (voicer 16 f) * control_m kr "gain" 1 (0,4,"amp")

---- ; send init voice
import qualified Sound.Sc3.Data.Yamaha.DX7 as DX7 {- hsc3-data -}
import qualified Sound.Sc3.Data.Yamaha.DX7.Sc3 as DX7 {- hsc3-data -}
DX7.dx7_sc3_data_load 400 [DX7.dx7_init_voice]

---- ; load patch data from .hex.text file
hex_fn = "/home/rohan/uc/the-center-is-between-us/trees/text/dx7/tc.hex.text"
v <- DX7.dx7_load_hex hex_fn
DX7.dx7_sc3_data_load 400 v
length v == 221

---- ; load patch data from .syx sysex file
syx_fn = "/home/rohan/sw/hsc3-data/data/yamaha/dx7ii/rom/DX7II-32A.syx"
bnk <- DX7.dx7_load_fmt9_sysex_err syx_fn
DX7.dx7_sc3_data_load 400 bnk
length bnk == 32

---- ; send external control messages
import Sound.OSC {- hosc -}
withSc3 (sendMessage (n_set1 1 "vc" 0))
withSc3 (sendMessage (n_set1 1 "gate" 1.0))
withSc3 (sendMessage (n_set1 1 "gate" 0.0))
withSc3 (sendMessage (n_set1 1 "data" 0.0))
withSc3 (sendMessage (n_set1 1 "mnn" 69.0))
withSc3 (sendMessage (n_set1 1 "vel" 10.0))

{---- DX7

An Sc3 UGen of the
[Levien](https://github.com/google/music-synthesizer-for-android) /
[Gauthier](https://github.com/asb2m10/dexed) DX7 emulator codes.

-}
