-- diskIn ; requires=dsk
let nc = 2 in diskIn nc (control kr "dsk" 0) Loop

---- ; setup
{fn = "/home/rohan/rd/data/ut/inland/flac/c/20.2/20.2-LW+RD.flac";nc = 2;dsk = 0}
withSC3 (mapM_ async [b_alloc dsk 65536 nc,b_read dsk fn 0 (-1) 0 True])
withSC3 (mapM_ async [b_close dsk,b_free dsk])
