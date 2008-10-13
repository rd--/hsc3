module Sound.SC3.UGen.DiskIO where

import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.UGen

-- | Stream soundfile from disk.
--
--  [@nc@] Number of channels in buffer/soundfile.
--
--  [@bufnum@] Buffer used for streaming (the file descriptor has to be left
--             open, see the @/b_read@ server command).
--
--  [@loop@] Whether to loop playback (0, 1)
--
diskIn :: Int -> UGen -> UGen -> UGen
diskIn nc bufnum loop = mkOsc AR "DiskIn" [bufnum, loop] nc

-- | Stream soundfile from disk with variable playback rate.
--
--  [@nc@] Number of channels in buffer/soundfile.
--
--  [@bufnum@] Buffer used for streaming (the file descriptor has to be left
--             open, see the @/b_read@ server command).
--
--  [@rate@] Playback rate
--
--  [@loop@] Whether to loop playback (0,1)
--
vDiskIn :: Int -> UGen -> UGen -> UGen -> UGen
vDiskIn nc bufnum rate loop = mkOsc AR "VDiskIn" [bufnum, rate, loop] nc
