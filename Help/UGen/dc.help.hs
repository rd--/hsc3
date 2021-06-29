-- dc ; zero
dc ar 0

-- dc ; offset, clicks on start and finish
dc ar 0.5

-- dc ; offset, clicks on start and finish
dc ar 0.5 + sinOsc ar 440 0 * 0.1

-- dc ; transient before LeakDC adapts and suppresses the offset
leakDC (dc ar 1) 0.995
