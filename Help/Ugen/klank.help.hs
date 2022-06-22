-- klank ; the klankSpec family of functions can help create the 'spec' entry
let impulse_freq = 0.125
    mk reson_freq = let ampl = 1
                        decay_time = 16
                        rp = replicate (length reson_freq)
                        k = klankSpec_k reson_freq (rp ampl) (rp decay_time)
                    in klank (impulse ar impulse_freq 0 * 0.1) 1 0 1 k
in mce (map mk [[221,614,1145,1804,2577,3456,4419]
               ,[977,1003,1390,1414,1432,1465,1748,1834,1919,1933,1987,2096,2107
                ,2202,2238,2280,2400,2435,2507,2546,2608,2652,2691,2708]]) * 0.1

-- klank ; there is a limited form of mce
let u = [1,1,1,1]
    p = [200,171,153,172]
    q = [930,971,953,1323]
    r = [8900,16062,9013,7892]
    k = mce [klankSpec_k p u u,klankSpec_k q u u,klankSpec_k r u u]
    s = mceTranspose k
    i = mce [2,2.07,2.13]
    t = impulse ar i 0 * 0.1
    l = mce [-1,0,1]
in mix (pan2 (klank t 1 0 1 s) l 0.25)

---- ; modal data
import Sound.SC3.Data.Modal {- hsc3-data -}
Just reson_freq = lookup "Spinel sphere (diameter=3.6675mm)" modal_frequencies
