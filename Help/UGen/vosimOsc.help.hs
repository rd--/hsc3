-- vosimOsc
let freq = sinOsc KR (sinOsc KR 0.01 0) 0 `in_exprange` (10,800)
    form1freq = sinOsc KR (mce2 0.5 0.1) 0 `in_exprange` (100,1000)
    form2freq = sinOsc KR 0.39 0 `in_exprange` (100,1000)
    shape = sinOsc KR 10 0
in X.vosimOsc AR freq form1freq form2freq shape * 0.1
