module Sound.SC3.UGen.FFT where

import Sound.OpenSoundControl (OSC)
import Sound.SC3.Server.Command (b_gen)
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.UGen

-- | Fast fourier transform.
fft :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
fft buf i h wt a = mkOsc KR "FFT" [buf,i,h,wt,a] 1

-- | Variant FFT constructor with default values for hop size, window
-- | type, and active status.
fft' :: UGen -> UGen -> UGen
fft' buf i = fft buf i 0.5 0 1

-- | Inverse Fast Fourier Transform.
ifft :: UGen -> UGen -> UGen
ifft buf wt = mkOsc AR "IFFT" [buf,wt] 1

-- | Variant ifft with default value for window type.
ifft' :: UGen -> UGen
ifft' buf = ifft buf 0

-- | Strict convolution of two continuously changing inputs.
convolution :: UGen -> UGen -> UGen -> UGen
convolution i kernel frameSize = mkOsc AR "Convolution" [i, kernel, frameSize] 1

-- | Pack demand-rate FFT bin streams into an FFT chain.
packFFT :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
packFFT b sz from to z mp = mkOscMCE KR "PackFFT" [b, sz, from, to, z, n] mp 1
    where n = constant (mceDegree mp)

-- | Format magnitude and phase data data as required for packFFT.
packFFTSpec :: [UGen] -> [UGen] -> UGen
packFFTSpec m p = mce (interleave m p)
    where interleave x y = concat (zipWith (\a b -> [a,b]) x y)

pvcollect :: UGen -> UGen -> (UGen -> UGen -> UGen -> (UGen, UGen)) -> UGen -> UGen -> UGen -> UGen
pvcollect c nf f from to z = packFFT c nf from to z mp
  where m = unpackFFT c nf from to 0
        p = unpackFFT c nf from to 1
        i = [from .. to]
        e = zipWith3 f m p i
        mp = (uncurry packFFTSpec) (unzip e)

pv_Add :: UGen -> UGen -> UGen
pv_Add ba bb = mkOsc KR "PV_Add" [ba,bb] 1

-- | Shift and scale the bin positions.
pv_BinShift :: UGen -> UGen -> UGen -> UGen
pv_BinShift buf str shift = mkOsc KR "PV_BinShift" [buf,str,shift] 1

-- | Combine low and high bins from two inputs.
pv_BinWipe :: UGen -> UGen -> UGen -> UGen
pv_BinWipe ba bb wp = mkOsc KR "PV_BinWipe" [ba,bb,wp] 1

-- | Clear bins above or below a cutoff point.
pv_BrickWall :: UGen -> UGen -> UGen
pv_BrickWall buf wp = mkOsc KR "PV_BrickWall" [buf,wp] 1

pv_ConformalMap :: UGen -> UGen -> UGen -> UGen
pv_ConformalMap buf real imag = mkOsc KR "PV_ConformalMap" [buf,real,imag] 1

-- | Copies spectral frame.
pv_Copy :: UGen -> UGen -> UGen
pv_Copy ba bb = mkOsc KR "PV_Copy" [ba,bb] 1

pv_CopyPhase :: UGen -> UGen -> UGen
pv_CopyPhase ba bb = mkOsc KR "PV_CopyPhase" [ba,bb] 1

pv_Diffuser :: UGen -> UGen -> UGen
pv_Diffuser buf trg = mkOsc KR "PV_Diffuser" [buf,trg] 1

pv_HainsworthFoote :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pv_HainsworthFoote buf h f thr wait = mkOsc AR "PV_HainsworthFoote" [buf,h,f,thr,wait] 1

pv_JensenAndersen :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pv_JensenAndersen buf sc hfe hfc sf thr wait = mkOsc AR "PV_JensenAndersen" [buf,sc,hfe,hfc,sf,thr,wait] 1

pv_LocalMax :: UGen -> UGen -> UGen
pv_LocalMax buf thr = mkOsc KR "PV_LocalMax" [buf,thr] 1

pv_MagAbove :: UGen -> UGen -> UGen
pv_MagAbove buf thr = mkOsc KR "PV_MagAbove" [buf,thr] 1

pv_MagBelow :: UGen -> UGen -> UGen
pv_MagBelow buf thr = mkOsc KR "PV_MagBelow" [buf,thr] 1

pv_MagClip :: UGen -> UGen -> UGen
pv_MagClip buf thr = mkOsc KR "PV_MagClip" [buf,thr] 1

pv_MagFreeze :: UGen -> UGen -> UGen
pv_MagFreeze buf frz = mkOsc KR "PV_MagFreeze" [buf,frz] 1

pv_MagMul :: UGen -> UGen -> UGen
pv_MagMul ba bb = mkOsc KR "PV_MagMul" [ba,bb] 1

pv_MagNoise :: UGen -> UGen
pv_MagNoise buf = mkOsc KR "PV_MagNoise" [buf] 1

pv_MagShift :: UGen -> UGen -> UGen -> UGen
pv_MagShift buf str shift = mkOsc KR "PV_MagShift" [buf,str,shift] 1

pv_MagSmear :: UGen -> UGen -> UGen
pv_MagSmear buf bins = mkOsc KR "PV_MagSmear" [buf,bins] 1

pv_MagSquared :: UGen -> UGen
pv_MagSquared buf = mkOsc KR "PV_MagSquared" [buf] 1

pv_Max :: UGen -> UGen -> UGen
pv_Max ba bb = mkOsc KR "PV_Max" [ba,bb] 1

pv_Min :: UGen -> UGen -> UGen
pv_Min ba bb = mkOsc KR "PV_Min" [ba,bb] 1

pv_Mul :: UGen -> UGen -> UGen
pv_Mul ba bb = mkOsc KR "PV_Mul" [ba,bb] 1

pv_PhaseShift270 :: UGen -> UGen
pv_PhaseShift270 buf = mkOsc KR "PV_PhaseShift270" [buf] 1

pv_PhaseShift90 :: UGen -> UGen
pv_PhaseShift90 buf = mkOsc KR "PV_PhaseShift90" [buf] 1

pv_PhaseShift :: UGen -> UGen -> UGen
pv_PhaseShift buf shift = mkOsc KR "PV_PhaseShift" [buf,shift] 1

pv_RectComb2 :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pv_RectComb2 ba bb teeth phase width = mkOsc KR "PV_RectComb2" [ba,bb,teeth,phase,width] 1

pv_RectComb :: UGen -> UGen -> UGen -> UGen -> UGen
pv_RectComb buf teeth phase width = mkOsc KR "PV_RectComb" [buf,teeth,phase,width] 1

unpack1FFT :: UGen -> UGen -> UGen -> UGen -> UGen
unpack1FFT buf size index which = mkOsc DR "Unpack1FFT" [buf, size, index, which] 1

unpackFFT :: UGen -> UGen -> UGen -> UGen -> UGen -> [UGen]
unpackFFT c nf from to w = map (\i -> unpack1FFT c nf i w) [from .. to]

-- * Partitioned convolution

-- | Calculate size of accumulation buffer given FFT and IR sizes.
pc_calcAccumSize :: Int -> Int -> Int
pc_calcAccumSize fft_size ir_length =
    let partition_size = fft_size `div` 2
        num_partitions = (ir_length `div` partition_size) + 1
    in fft_size * num_partitions

-- | Generate accumulation buffer given time-domain IR buffer and FFT size.
pc_preparePartConv :: Int -> Int -> Int -> OSC
pc_preparePartConv b irb fft_size =
    b_gen b "PreparePartConv" (map fromIntegral [irb, fft_size])

-- | Partitioned convolution.
partConv :: UGen -> UGen -> UGen -> UGen -> UGen
partConv i sz ib ab = mkOsc AR "PartConv" [i,sz,ib,ab] 1

-- Local Variables:
-- truncate-lines:t
-- End:
