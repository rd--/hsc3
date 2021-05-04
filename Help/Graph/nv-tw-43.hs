-- https://swiki.hfbk-hamburg.de/MusicTechnology/899 (nv) L43
let f i =
      let x = impulse AR 1 (i / 10) + impulse AR 0 0
          o = lfSaw AR (mce2 102 101) 0
          d = 1 / latch (((1.015 ** sweep (dc AR 0) 1 * 64) `modE` 1 + 1) * 200) x
      in pluck o x 1 d 4 0.2
in mceMean (mceFill 10 f) * 0.25
