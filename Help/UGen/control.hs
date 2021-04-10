-- control
sinOsc AR (control KR "freq" 440) 0 * control KR "amp" 0.1

---- ; command list to send messages to set oscillator frequency
UI.ui_scsynth_command_list (map (\x -> n_set 1 [("freq",55 * (x ** 2)),("amp",0.1 * (1 / x))]) [1 .. 7])
