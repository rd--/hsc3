-- dc ; zero
dc AR 0

-- dc ; offset, clicks on start and finish
dc AR 0.5

-- dc ; offset, clicks on start and finish
dc AR 0.5 + sinOsc AR 440 0 * 0.1

-- dc ; transient before LeakDC adapts and suppresses the offset
leakDC (dc AR 1) 0.995
