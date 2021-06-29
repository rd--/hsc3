-- http://fredrikolofsson.com/f0blog/?q=node/478 (f0)
let a = sinOscFB ar
    k = 1/9
    f i = let a0 = a (i+1) k * 999
              a1 = a k 1 * k
              a2 = a a0 k * a1
              a3 = a 0.1 3
              a4 = a a2 a3 * ((i+2) * 999)
              a5 = a k k
          in a a4 a5 * k
    s = sum (map f [0..9])
in mce2 s s * 0.1
