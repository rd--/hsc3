-- http://www.fredrikolofsson.com/f0blog/?q=node/490 (f0)
let sosc fr = sinOsc AR fr 0
    f0 = mce3 100 200 300 + sosc (mce3 0.11 0.22 0.33)
    ph0 = sosc (mce3 0.1 0.2 0.3) * 2 * pi
    a0 = 0.1 + sosc (mce3 0.01 0.02 0.03) * 0.05
    i = sinOsc AR f0 ph0 * a0
    w = sosc (sosc (sosc 0.13 * 5 + 6) * 8 + 50)
    s = splay i w 0.7 (sosc 1.2 * 0.6) True
in out 0 (gVerb s 20 5 1 0.5 25 0 1 1 30)
