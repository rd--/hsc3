-- vDiskIn
let b = control KR "buf" 0
    nc = 2
in vDiskIn nc 0 (sinOsc KR 0.25 0 * 0.25 + 1) Loop 0

---- ; allocate and read file, leave open
let fn = "/home/rohan/data/audio/pf-c5.aif"
let nc = 2
withSC3 (mapM_ async [b_alloc 0 8192 nc,b_read 0 fn 0 (-1) 0 True])
