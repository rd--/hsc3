-- https://swiki.hfbk-hamburg.de/MusicTechnology/899 (nv) L43
let k = 2
    o = lfSaw AR (mce2 102 101) 0
    f i =
      let x = impulse AR 1 (i / 10) + impulse AR 0 0
          d = latch (((1.015 ** sweep (dc AR 0) 1 * 64) `modE` 1 + 1) * 200) x
      in pluck o x 1 (1 / d) 4 0.2
in mceMean (mceFill k f) * 0.25
