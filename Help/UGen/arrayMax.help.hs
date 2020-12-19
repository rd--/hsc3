-- arrayMax
let son = sinOsc AR (mce [100, 100.3 .. 110]) 0
    (val,_ix) = unmce2 (X.arrayMax son)
in mce2 (son * 0.02) (leakDC val 0.995 * 0.02) -- the operation tends to induce DC offset
