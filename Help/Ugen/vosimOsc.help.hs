-- vosimOsc
let freq = sinOsc kr (sinOsc kr 0.01 0) 0 `in_exprange` (10,800)
    form1freq = sinOsc kr (mce2 0.5 0.1) 0 `in_exprange` (100,1000)
    form2freq = sinOsc kr 0.39 0 `in_exprange` (100,1000)
    shape = sinOsc kr 10 0
in X.vosimOsc ar freq form1freq form2freq shape * 0.1

-- vosimOsc ; event control
let f (_,g,x,y,z,o,rx,ry,_,_,_) =
      let freq = x * 40 + 10
          form1freq = y * 900 + 100
          form2freq = rx * 900 + 100
          shape = ry
      in pan2 (X.vosimOsc ar freq form1freq form2freq shape) (o * 2 - 1) (g * z)
in mix (voicer 16 f) * control kr "gain" 1
