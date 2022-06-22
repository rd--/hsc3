-- vDiskIn
let b = control kr "buf" 0
    nc = 2
in vDiskIn nc 0 (sinOsc kr 0.25 0 * 0.25 + 1) Loop 0

---- ; allocate and read file, leave open
let fn = sfResolve "pf-c5.aif"
let nc = sfNumChannels fn
withSc3 (mapM_ async [b_alloc 0 8192 nc,b_read 0 fn 0 (-1) 0 True])

