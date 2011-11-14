-- | Disk file input and output UGens.
module Sound.SC3.UGen.DiskIO where

import Sound.SC3.UGen.Enum
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.UGen
import Sound.SC3.UGen.Utilities

-- | Stream soundfile from disk.
--
--  [@nc@] Number of channels in buffer/soundfile.
--
--  [@bufnum@] Buffer used for streaming (the file descriptor has to be left
--             open, see the @/b_read@ server command).
--
--  [@loop@] Whether to loop playback (0, 1)
diskIn :: Int -> UGen -> Loop -> UGen
diskIn nc bufnum loop = mkOsc AR "DiskIn" [bufnum, from_loop loop] nc

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
vDiskIn :: Int -> UGen -> UGen -> Loop -> UGen
vDiskIn nc bufnum rate loop = mkOsc AR "VDiskIn" [bufnum, rate, from_loop loop] nc

-- | Stream soundfile to disk.
--
--  [@bufnum@] Buffer used for streaming (the file descriptor has to be left
--             open, see the @/b_write@ server command).
--
--  [@output@] Current number of written frames.
diskOut :: UGen -> UGen -> UGen
diskOut bufnum inputs = mkOscMCE AR "DiskOut" [bufnum] inputs 1
