-- f0 <https://twitter.com/redFrik/status/1367808902457397250>
-- there is a subtle error in this translation i cannot see, the only difference appears to be the shared (impulse AR 0 0) node
let rat = [76.1, 76.1, 64, 57, 38.1, 85.4, 32, 114, 42.7, 47.9, 95.9]
    f i =
      let i_ = constant i
          f0 = (rat !! i) * (sinOsc AR (1/16) 0 `greater_than` 0 * 2 + 6 - (sinOsc AR (sinOsc AR (1/32) i_) 0 / 20))
          z = sinOsc AR f0 0 / 9
          x = varLag_env (floorE (sinOsc AR (1/64) 0 * 6 + 6)) 0.1 (EnvNum 0) Nothing
          y = varLag_env (sinOsc AR (sinOsc AR (1/4) (i_ / 11 * pi) `less_than` 0 * 2) 0 * 0.1) 0.01 (EnvNum (sinOsc AR 0.01 i_)) Nothing
          dly = (i_ + x) `modE` 11 / 33 + 0.1 + y
          dcy = sinOsc AR (1/9) 0 + 1
      in combC z 0.5 dly dcy
in tanh (splay (mce (map f [0 .. 10])) 1 1 0 True)
