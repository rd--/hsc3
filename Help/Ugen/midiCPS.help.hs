-- midiCps
saw ar (midiCps (line kr 24 108 10 DoNothing)) * 0.05

-- midiCps ; step
saw ar (midiCps (roundE (line kr 24 108 10 DoNothing))) * 0.05
