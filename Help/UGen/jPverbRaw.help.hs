-- jPverbRaw ; default values
let (i1,i2) = (soundIn 0,soundIn 1)
in X.jPverbRaw i1 i2 0 0.707 2000 1 500 1 0.1 2 1 1 1

-- jPverbRaw ; dreamverb
let (i1,i2) = (soundIn 0,soundIn 1)
    rvb = X.jPverbRaw i1 i2 0.314 0.421 1024.0219 0.0 2450.082 0.843 4.639 0.103 0.706 2.793 60.0
in mce2 i1 i2 + rvb

-- ; jPverbRaw ; dreamverb ; controls
let k = control KR
    (i1,i2) = (soundIn 0,soundIn 1)
    rvb = X.jPverbRaw i1 i2 (k "damp" 0.314) (k "earlydiff" 0.421) (k "highband" 1024.02) (k "highx" 0.0) (k "lowband" 2450.08) (k "lowx" 0.844) (k "mdepth" 4.639) (k "mfreq" 0.103) (k "midx" 0.706) (k "size" 2.794) (k "t60" 60)
in mce2 i1 i2 + rvb
