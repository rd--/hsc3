-- midiCPS
saw ar (midiCPS (line kr 24 108 10 DoNothing)) * 0.05

-- midiCPS ; step
saw ar (midiCPS (roundE (line kr 24 108 10 DoNothing))) * 0.05
