-- ctl8Voicer ; 16 voices of 8 control inputs ; control inputs are (0-1)
let f _ (c1,c2,c3,c4,c5,c6,c7,_) =
      let freq = midiCps (48 + c1 * 24)
          nh = c2 * 200
          pan = sinOsc kr (c3 * 2) 0 * c4
          amp = c5 + (sinOsc kr c6 0 * c7)
      in pan2 (blip ar freq nh) pan amp
in mix (ctl8Voicer 16 f) * control kr "gain" 0.1

-- control inputs calculated as offsets per voice ; equivalent to above
let f i =
      let cc n = in' 1 kr (11000 + constant (i * 8 + n - 1))
          freq = midiCps (48 + cc 1 * 24)
          nh = cc 2 * 200
          pan = sinOsc kr (cc 3 * 2) 0 * cc 4
          amp = cc 5 + (sinOsc kr (cc 6) 0 * cc 7)
      in pan2 (blip ar freq nh) pan amp
in sum (map f [0 .. 15]) * control kr "gain" 0.1

-- ctl16Voicer ; 8 voices of 16 control inputs ; applied at inputs as above ignores every other 8-block
let f _ ((c1,c2,c3,c4,c5,c6,c7,_),_) =
      let freq = midiCps (48 + c1 * 24)
          nh = c2 * 200
          pan = sinOsc kr (c3 * 2) 0 * c4
          amp = c5 + (sinOsc kr c6 0 * c7)
      in pan2 (blip ar freq nh) pan amp
in mix (ctl16Voicer 8 f) * control kr "gain" 0.1

---- ; initial settings ; 16 groups of eight
x = [[0.483,0.008,0.396,0.006,0.128,0.331,0.196,0],[0.005,0.015,0.194,0.481,0.037,0.052,0.189,0],[0.008,0.014,0.925,0.064,0.269,0.117,0.357,0],[0.79,0.009,0.187,0.216,0.14,0.064,0.046,0],[0.036,0.015,0.15,1,0.139,0.106,0.142,0],[0.065,0.014,0.683,1,0.208,0.108,0.042,0],[0.464,0.01,0.824,1,0.024,0.171,0.06,0],[0.498,0.018,0.828,0.066,0,0,0,0],[0.011,0.012,0.129,0,0,0,0,0],[0.07,0.015,0.309,0.162,0,0,0,0],[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0],[0.479,0.01,0.173,0.29,0,0.015,0.265,0],[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0],[0.286,0.011,0.027,0.39,0.238,0,0,0]]

---- ; send initial settings
import Sound.OSC {- hosc -}
withSc3 (sendMessage (c_setn1 (11000, replicate (16 * 8) 0)))
withSc3 (sendBundle (bundle immediately (map c_setn1 (zip [11000, 11008..] x))))
(length x == 16, map length x == replicate 16 8, 16 * 8 == 128)
