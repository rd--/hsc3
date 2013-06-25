-- Sound.SC3.UGen.Help.viewSC3Help "LocalIn"
-- Sound.SC3.UGen.DB.ugenSummary "LocalIn"

import Sound.SC3.ID

noise_signal =
    let e = decay (impulse AR 0.3 0) 0.1
    in whiteNoise 'α' AR * e * 0.2

outside_world = soundIn 4

-- audition (out 0 (ping_pong noise_signal))
-- audition (out 0 (ping_pong outside_world))
ping_pong z =
    let a1 = localIn 2 AR + mce [z,0]
        a2 = delayN a1 0.2 0.2
        a3 = mceEdit reverse a2 * 0.8
    in mrg [z + a2,localOut a3]

rotate2_mce z p =
    case mceChannels z of
      [l,r] -> rotate2 l r p
      _ -> error "rotate2_mce"

-- audition (out 0 (tape_delay 0.35 1.20 noise_signal))
-- audition (out 0 (tape_delay 0.25 1.25 outside_world))
tape_delay dt fb z =
    let a = amplitude KR (mix z) 0.01 0.01
        z' = z * (a >* 0.02)
        l0 = localIn 2 AR
        l1 = onePole l0 0.4
        l2 = onePole l1 (-0.08)
        l3 = rotate2_mce l2 0.2
        l4 = delayN l3 dt dt
        l5 = leakDC l4 0.995
        l6 = softClip ((l5 + z') * fb)
    in mrg2 (l6 * 0.1) (localOut l6)
