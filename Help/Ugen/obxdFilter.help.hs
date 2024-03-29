-- obxdFilter
let cutoff = mouseX kr 110.0 (110.0 * 16) Exponential 0.2
    resonance = 0.0
in X.obxdFilter (pinkNoiseId 'α' ar * 0.5) cutoff resonance 0.0 0.0 0.0

-- obxdFilter
let cutoff = mouseX kr 110.0 (110.0 * 16) Exponential 0.2
    resonance = mouseY kr 0.0 1.10 Linear 0.2
in X.obxdFilter (pinkNoiseId 'α' ar * 0.5) cutoff resonance 0.5 0.0 0.0

-- obxdFilter ; multimode selects the filter mode, the range is (0,1), there are four modes.
let cutoff = mouseX kr 110.0 (110.0 * 16) Exponential 0.2
    multimode = mouseY kr 0.0 1.0 Linear 0.2
    fourpole = 1.0
in X.obxdFilter (pinkNoiseId 'α' ar * 0.5) cutoff 0.0 multimode 0.0 fourpole

-- obxdFilter ; controls
let k = control kr
    c1 = k "cutoff" 440.0
    c2 = k "resonance" 0.0
    c3 = k "multimode" 0.5
    c4 = k "bandpass" 0.0
    c5 = k "fourpole" 0.0
in X.obxdFilter (pinkNoiseId 'α' ar * 0.5) c1 c2 c3 c4 c5

---- ; obxdFilter ; set controls
let set k v = withSc3 (Sound.Osc.sendMessage (n_set1 (-1) k v))
(set "cutoff" 880.0)
(set "resonance" 0.95)
(set "multimode" 0.75)
(set "bandpass" 1.0)
(set "fourpole" 1.0)

{---- RObxdFilter

cutoff    : FREQUENCY (CPS)         :
resonance : Q (0.0 - 1.0)           :
multimode : FILTER MODE (0.0 - 1.0) : LOW-NOTCH/BANDPASS-HIGH | 24DB - 6DB LOWPASS
bandpass  : 0 = FALSE, 1 = TRUE     : 0 = NOTCH, 1 = BANDPASS
fourpole  : 0 = FALSE, 1 = TRUE     : 0 = 12/DB MULTI-MODE, 1 = 24/DB LOWPASS

-}
