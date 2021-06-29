-- xy-interference (rd, 2006-10-28)
let xy_interference_m :: UId m => m UGen
    xy_interference_m = do
      let x = mouseX kr 20 22000 Linear (mce2 0.005 0.025)
          y = mouseY kr 20 22000 Linear (mce2 0.005 0.075)
          nd = do n <- lfNoise0M kr (mce2 5 9)
                  let a = sinOsc ar (x + n) 0
                      b = sinOsc ar y 0
                  return (a * b)
      fmap sum_opt (sequence (replicate 3 nd))
in uid_st_eval xy_interference_m * 0.5
