-- fos ; same as OnePole
let x = lfTri AR 0.4 0 * 0.99
in fos (lfSaw AR 200 0 * 0.1) (1 - (abs x)) 0 x

-- fos ;same as OneZero
let x = lfTri AR 0.4 0 * 0.99
in fos (lfSaw AR 200 0 * 0.1) (1 - (abs x)) x 0
