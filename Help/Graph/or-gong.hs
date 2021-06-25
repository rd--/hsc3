-- or ; https://recarteblog.wordpress.com/2021/05/05/gongfm_sc
let ratio1 = rand 'α' 0.49 1.11 -- 0.9
    ratio2 = rand 'β' 0.17 0.55 -- 0.55
    ratio3 = rand 'γ' 0.34 1.35 -- 1.03
    index2 = rand 'δ' 1.33 2 -- 2
    index3 = rand 'ε' 3.29 5.06 -- 4
    amp = 0.1
    dur = rand 'ζ' 3 9 -- 7
    pan = 0
    mnn = mce [67, 70, 74]
    g = 1
    mk_env l t = envGen KR g 1 0 1 RemoveSynth (envelope l t [])
    env3 = mk_env [0, 1, 1, 0] [0.4, 0.3, dur]
    env2 = mk_env [0, 1, 1, 0] [0, 0.3, dur]
    env1 = mk_env [0, 1, 1, 0] [0.003, 0.3, dur - 0.5]
    op3 = sinOsc AR (midiCPS (mnn) * ratio3) 0 * midiCPS (mnn) * ratio3 * index3 * env3
    op2 = sinOsc AR (midiCPS (mnn) * ratio2 + op3) 0 * midiCPS (mnn) * ratio2 * index2 * env2
    op1 = sinOsc AR (midiCPS (mnn) * ratio1 + op2) 0
in splay (op1 * env1 * amp) 1 1 0 True

-- or ; https://recarteblog.wordpress.com/2021/05/05/gongfm_sc ; event control
let f _ (g,x,y,z,o,rx,ry,_,_,_) =
      let ratio1 = x `in_range` (0.49,1.11)
          ratio2 = y `in_range` (0.15,0.55)
          ratio3 = o `in_range` (0.34,1.35)
          index2 = rx `in_range` (1.33,2)
          index3 = ry `in_range` (3.29,5.06)
          amp = latch z g
          dur = y * 6 + 3
          pan = 0
          mnn = mce [67, 70, 74] * (0.5 + x)
          mk_env l t = envGen KR g 1 0 1 DoNothing (envelope l t [])
          env3 = mk_env [0, 1, 1, 0] [0.4, 0.3, dur]
          env2 = mk_env [0, 1, 1, 0] [0, 0.3, dur]
          env1 = mk_env [0, 1, 1, 0] [0.003, 0.3, dur - 0.5]
          op3 = sinOsc AR (midiCPS (mnn) * ratio3) 0 * midiCPS (mnn) * ratio3 * index3 * env3
          op2 = sinOsc AR (midiCPS (mnn) * ratio2 + op3) 0 * midiCPS (mnn) * ratio2 * index2 * env2
          op1 = sinOsc AR (midiCPS (mnn) * ratio1 + op2) 0
      in splay (op1 * env1 * amp) 1 1 0 True
in mix (eventVoicer 16 f) * control KR "gain" 0.25
