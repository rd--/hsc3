-- dconst ; fast notes of random duration for 0.5 seconds, then a single note for 0.5 seconds
let dexprandId z rp l r = lin_exp (dwhiteId z rp 0 1) 0 1 l r -- there is no Dexprand
    t = dconstId 'α' 0.5 (dwhiteId 'β' dinf 0.05 0.08) 0.001
    f = duty kr (dseqId 'γ' dinf (mce2 t 0.5)) 0 DoNothing (dexprandId 'δ' dinf 200 600)
in varSaw ar (lag f 0.02) 0 0.3 * 0.1
