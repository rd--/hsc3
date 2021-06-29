-- control
sinOsc ar (control kr "freq" 440) 0 * control kr "amp" 0.1

-- control ; ir and kr and tr controls ; setting amp also triggers an envelope
let phase1 = control ir "phase1" 0
    phase2 = control ir "phase2" (pi / 4)
    freq1 = control kr "freq1" 450
    freq2 = control kr "freq2" 900
    amp1 = tr_control "amp1" 0
    amp2 = tr_control "amp2" 0
    env = decay2 (impulse kr 1 0 * 0.1 + mce2 amp1 amp2) 0.01 0.2
in sinOsc ar (mce2 freq1 freq2) (mce2 phase1 phase2) * env

-- control ; constructor setting index sequence
let [f,a] = control_set [control kr "freq" 440
                        ,control kr "amp" 0.1]
in sinOsc ar f 0 * a

-- control ; contructor with control index input ; this will fail if n /= 1 (ie. 0 or 2)
let n = 1
    f = control_f64 kr (Just 0) "freq" 440
    a = control_f64 kr (Just n) "amp" 0.1
    p = control_f64 kr Nothing "phase" 0
in sinOsc ar f p * a

-- trigControl ; graph with the three types of non-audio controls ; trigger controls are drawn cyan
let freq = control kr "freq" 440
    phase = control ir "phase" 0
    gate' = tr_control "gate" 1
    amp = control kr "amp" 0.1
    e = envGen kr gate' amp 0 1 DoNothing (envASR 0.01 1 1 EnvLin)
in sinOsc ar freq phase * e

---- ; set frequency and gate controls
withSC3 (sendMessage (n_set1 1 "freq" 2200))
withSC3 (sendMessage (n_set1 1 "gate" 1))

---- ; command list ui to send messages to set oscillator frequency
UI.ui_scsynth_command_list (map (\x -> n_set 1 [("freq1",55 * (x ** 2)),("amp1",0.1 * (1 / x))]) [1 .. 7])
