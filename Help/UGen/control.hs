-- control
sinOsc AR (control KR "freq" 440) 0 * control KR "amp" 0.1

-- control ; ir and kr and tr controls ; setting amp also triggers an envelope
let phase1 = control IR "phase1" 0
    phase2 = control IR "phase2" (pi / 4)
    freq1 = control KR "freq1" 450
    freq2 = control KR "freq2" 900
    amp1 = tr_control "amp1" 0
    amp2 = tr_control "amp2" 0
    env = decay2 (impulse KR 1 0 * 0.1 + mce2 amp1 amp2) 0.01 0.2
in sinOsc AR (mce2 freq1 freq2) (mce2 phase1 phase2) * env

-- control ; constructor setting index sequence
let [f,a] = control_set [control KR "freq" 440
                        ,control KR "amp" 0.1]
in sinOsc AR f 0 * a

-- control ; contructor with control index input ; this will fail if n /= 1 (ie. 0 or 2)
let n = 1
    f = control_f64 KR (Just 0) "freq" 440
    a = control_f64 KR (Just n) "amp" 0.1
    p = control_f64 KR Nothing "phase" 0
in sinOsc AR f p * a

---- ; command list to send messages to set oscillator frequency
UI.ui_scsynth_command_list (map (\x -> n_set 1 [("freq1",55 * (x ** 2)),("amp1",0.1 * (1 / x))]) [1 .. 7])
