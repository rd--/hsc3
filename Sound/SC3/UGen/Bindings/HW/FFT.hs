-- | Frequency domain unit generators.
module Sound.SC3.UGen.FFT where

import Sound.SC3.UGen.Buffer
import Sound.SC3.UGen.Identifier
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.Type
import Sound.SC3.UGen.UGen

-- | Fast fourier transform.
fft :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fft buf i h wt a ws = mkOsc KR "FFT" [buf,i,h,wt,a,ws] 1

-- | Outputs signal for @FFT@ chains, without performing FFT.
fftTrigger :: UGen -> UGen -> UGen -> UGen
fftTrigger b h p = mkOsc KR "FFTTrigger" [b,h,p] 1

-- | Inverse Fast Fourier Transform.
ifft :: UGen -> UGen -> UGen -> UGen
ifft buf wt ws = mkOsc AR "IFFT" [buf,wt,ws] 1

-- | Strict convolution of two continuously changing inputs.
convolution :: UGen -> UGen -> UGen -> UGen
convolution i kernel frameSize = mkOsc AR "Convolution" [i, kernel, frameSize] 1

-- | Real-time fixed kernel convolver.
convolution2 :: UGen -> UGen -> UGen -> UGen -> UGen
convolution2 in_ kernel trigger framesize = mkOsc AR "Convolution2" [in_,kernel,trigger,framesize] 1

-- | Real-time convolver with linear interpolation
convolution2L :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
convolution2L in_ kernel trigger framesize crossfade = mkOsc AR "Convolution2L" [in_,kernel,trigger,framesize,crossfade] 1

-- | Time based convolver.
convolution3 :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
convolution3 rate in_ kernel trigger framesize = mkOscR [AR,KR] rate "Convolution3" [in_,kernel,trigger,framesize] 1

-- | Complex addition.
pv_Add :: UGen -> UGen -> UGen
pv_Add ba bb = mkOsc KR "PV_Add" [ba,bb] 1

-- | Shift and scale the bin positions.
pv_BinShift :: UGen -> UGen -> UGen -> UGen -> UGen
pv_BinShift buf str shift interp = mkOsc KR "PV_BinShift" [buf,str,shift,interp] 1

-- | Combine low and high bins from two inputs.
pv_BinWipe :: UGen -> UGen -> UGen -> UGen
pv_BinWipe ba bb wp = mkOsc KR "PV_BinWipe" [ba,bb,wp] 1

-- | Clear bins above or below a cutoff point.
pv_BrickWall :: UGen -> UGen -> UGen
pv_BrickWall buf wp = mkOsc KR "PV_BrickWall" [buf,wp] 1

-- | Complex plane attack.
pv_ConformalMap :: UGen -> UGen -> UGen -> UGen
pv_ConformalMap buf real imag = mkOsc KR "PV_ConformalMap" [buf,real,imag] 1

-- | Copies spectral frame.
pv_Copy :: UGen -> UGen -> UGen
pv_Copy ba bb = mkOsc KR "PV_Copy" [ba,bb] 1

-- | Copy magnitudes and phases.
pv_CopyPhase :: UGen -> UGen -> UGen
pv_CopyPhase ba bb = mkOsc KR "PV_CopyPhase" [ba,bb] 1

-- | Random phase shifting.
pv_Diffuser :: UGen -> UGen -> UGen
pv_Diffuser buf trg = mkOsc KR "PV_Diffuser" [buf,trg] 1

-- | FFT onset detector.
pv_HainsworthFoote :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pv_HainsworthFoote buf h f thr wt = mkOsc AR "PV_HainsworthFoote" [buf,h,f,thr,wt] 1

-- | FFT feature detector for onset detection.
pv_JensenAndersen :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pv_JensenAndersen buf sc hfe hfc sf thr wt = mkOsc AR "PV_JensenAndersen" [buf,sc,hfe,hfc,sf,thr,wt] 1

-- | Pass bins which are a local maximum.
pv_LocalMax :: UGen -> UGen -> UGen
pv_LocalMax buf thr = mkOsc KR "PV_LocalMax" [buf,thr] 1

-- | Pass bins above a threshold.
pv_MagAbove :: UGen -> UGen -> UGen
pv_MagAbove buf thr = mkOsc KR "PV_MagAbove" [buf,thr] 1

-- | Pass bins below a threshold.
pv_MagBelow :: UGen -> UGen -> UGen
pv_MagBelow buf thr = mkOsc KR "PV_MagBelow" [buf,thr] 1

-- | Clip bins to a threshold.
pv_MagClip :: UGen -> UGen -> UGen
pv_MagClip buf thr = mkOsc KR "PV_MagClip" [buf,thr] 1

-- | Freeze magnitudes.
pv_MagFreeze :: UGen -> UGen -> UGen
pv_MagFreeze buf frz = mkOsc KR "PV_MagFreeze" [buf,frz] 1

-- | Multiply magnitudes.
pv_MagMul :: UGen -> UGen -> UGen
pv_MagMul ba bb = mkOsc KR "PV_MagMul" [ba,bb] 1

-- | Multiply magnitudes by noise.
pv_MagNoise :: UGen -> UGen
pv_MagNoise buf = mkOsc KR "PV_MagNoise" [buf] 1

-- | Shift and stretch magnitude bin position.
pv_MagShift :: UGen -> UGen -> UGen -> UGen
pv_MagShift buf str shift = mkOsc KR "PV_MagShift" [buf,str,shift] 1

-- | Average magnitudes across bins.
pv_MagSmear :: UGen -> UGen -> UGen
pv_MagSmear buf bins = mkOsc KR "PV_MagSmear" [buf,bins] 1

-- | Square magnitudes.
pv_MagSquared :: UGen -> UGen
pv_MagSquared buf = mkOsc KR "PV_MagSquared" [buf] 1

-- | Maximum magnitude.
pv_Max :: UGen -> UGen -> UGen
pv_Max ba bb = mkOsc KR "PV_Max" [ba,bb] 1

-- | Minimum magnitude.
pv_Min :: UGen -> UGen -> UGen
pv_Min ba bb = mkOsc KR "PV_Min" [ba,bb] 1

-- | Complex multiply.
pv_Mul :: UGen -> UGen -> UGen
pv_Mul ba bb = mkOsc KR "PV_Mul" [ba,bb] 1

-- | Shift phase by 270 degrees.
pv_PhaseShift270 :: UGen -> UGen
pv_PhaseShift270 buf = mkOsc KR "PV_PhaseShift270" [buf] 1

-- | Shift phase by 90 degrees.
pv_PhaseShift90 :: UGen -> UGen
pv_PhaseShift90 buf = mkOsc KR "PV_PhaseShift90" [buf] 1

-- | Shift phase.
pv_PhaseShift :: UGen -> UGen -> UGen -> UGen
pv_PhaseShift buf shift integrate = mkOsc KR "PV_PhaseShift" [buf,shift,integrate] 1

-- | Make gaps in spectrum.
pv_RectComb2 :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pv_RectComb2 ba bb teeth phase width = mkOsc KR "PV_RectComb2" [ba,bb,teeth,phase,width] 1

-- | Make gaps in spectrum.
pv_RectComb :: UGen -> UGen -> UGen -> UGen -> UGen
pv_RectComb buf teeth phase width = mkOsc KR "PV_RectComb" [buf,teeth,phase,width] 1

-- | Unpack a single value (magnitude or phase) from an FFT chain
unpack1FFT :: UGen -> UGen -> UGen -> UGen -> UGen
unpack1FFT buf size index' which = mkOsc DR "Unpack1FFT" [buf, size, index', which] 1

-- | Partitioned convolution.
partConv :: UGen -> UGen -> UGen -> UGen
partConv i sz ib = mkOsc AR "PartConv" [i, sz, ib] 1

-- * NONDET

-- | Randomize order of bins.
pv_BinScramble :: ID i => i -> UGen -> UGen -> UGen -> UGen -> UGen
pv_BinScramble z buf wp width trg = mkOscId z KR "PV_BinScramble" [buf,wp,width,trg] 1

-- | Randomly clear bins.
pv_RandComb :: ID i => i -> UGen -> UGen -> UGen -> UGen
pv_RandComb z buf wp trg = mkOscId z KR "PV_RandComb" [buf,wp,trg] 1

-- | Cross fade, copying bins in random order.
pv_RandWipe :: ID i => i -> UGen -> UGen -> UGen -> UGen -> UGen
pv_RandWipe z ba bb wp trg = mkOscId z KR "PV_RandWipe" [ba,bb,wp,trg] 1

-- Local Variables:
-- truncate-lines:t
-- End:
