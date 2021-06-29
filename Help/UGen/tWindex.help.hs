-- tWindex
let p = mce [1/5, 2/5, 2/5]
    a = mce [400, 500, 600]
    t = impulse kr 6 0
    i = tWindex 'α' t 0 p
in sinOsc ar (select i  a) 0 * 0.1

-- tWindex ; modulating probability values
let p = mce [1/4, 1/2, sinOsc kr 0.3 0 * 0.5 + 0.5]
    a = mce [400, 500, 600]
    t = impulse kr 6 0
    i = tWindex 'α' t 1 p
in sinOsc ar (select i a) 0 * 0.1
