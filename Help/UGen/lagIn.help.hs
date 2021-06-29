-- lagIn ; oscillator reading frequency at control bus
sinOsc ar (lagIn 1 (control kr "bus" 10) 1) 0 * 0.1

---- ; set/reset frequency at control bus
import Sound.OSC {- hosc -}
withSC3 (sendMessage (c_set1 10 200))
withSC3 (sendMessage (c_set1 10 2000))
