-- diskIn ; requires=dsk ; c.f. sndfileIn
let (buf, nc) = (control kr "dsk" 0, 2) in diskIn nc buf Loop

---- ; setup & cleanup
{fn = sfResolve "20.2-LW+RD.flac";nc = 2;dsk = 0}
withSc3 (mapM_ async [b_alloc dsk 65536 2,b_read dsk fn 0 (-1) 0 True])
withSc3 (mapM_ async [b_close dsk,b_free dsk])
