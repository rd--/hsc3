module Hsc.FFT where

import Hsc.Rate (Rate(AR, KR))
import Hsc.Construct (mkOsc, mkOsc', uniquify)
import Hsc.UId (UId(UId))

fft  buf i = mkOsc KR "FFT"  [buf,i] 1 0
ifft buf   = mkOsc AR "IFFT" [buf]   1 0

pv_add ba bb = mkOsc KR "PV_Add" [ba,bb] 1 0
pv_binscramble buf wp width trg = mkOsc KR "PV_BinScramble" [buf,wp,width,trg] 1 0
pv_binshift buf str shift = mkOsc KR "PV_BinShift" [buf,str,shift] 1 0
pv_binwipe ba bb wp = mkOsc KR "PV_BinWipe" [ba,bb,wp] 1 0
pv_brickwall buf wp = mkOsc KR "PV_BrickWall" [buf,wp] 1 0
pv_conformalmap buf real imag = mkOsc KR "PV_ConformalMap" [buf,real,imag] 1 0
pv_copyphase ba bb = mkOsc KR "PV_CopyPhase" [ba,bb] 1 0
pv_diffuser buf trg = mkOsc KR "PV_Diffuser" [buf,trg] 1 0
pv_hainsworthfoote buf h f thr wait = mkOsc KR "PV_HainsworthFoote" [buf,h,f,thr,wait] 1 0
pv_jensenandersen buf sc hfe hfc sf thr wait = mkOsc KR "PV_JensenAndersen" [buf,sc,hfe,hfc,sf,thr,wait] 1 0
pv_localmax buf thr = mkOsc KR "PV_LocalMax" [buf,thr] 1 0
pv_magabove buf thr = mkOsc KR "PV_MagAbove" [buf,thr] 1 0
pv_magbelow buf thr = mkOsc KR "PV_MagBelow" [buf,thr] 1 0
pv_magclip buf thr = mkOsc KR "PV_MagClip" [buf,thr] 1 0
pv_magfreeze buf frz = mkOsc KR "PV_MagFreeze" [buf,frz] 1 0
pv_magmul ba bb = mkOsc KR "PV_MagMul" [ba,bb] 1 0
pv_magnoise buf = mkOsc KR "PV_MagNoise" [buf] 1 0
pv_magshift buf str shift = mkOsc KR "PV_MagShift" [buf,str,shift] 1 0
pv_magsmear buf bins = mkOsc KR "PV_MagSmear" [buf,bins] 1 0
pv_magsquared buf = mkOsc KR "PV_MagSquared" [buf] 1 0
pv_max ba bb = mkOsc KR "PV_Max" [ba,bb] 1 0
pv_min ba bb = mkOsc KR "PV_Min" [ba,bb] 1 0
pv_mul ba bb = mkOsc KR "PV_Mul" [ba,bb] 1 0
pv_phaseshift buf shift = mkOsc KR "PV_PhaseShift" [buf,shift] 1 0
pv_phaseshift270 buf = mkOsc KR "PV_PhaseShift270" [buf] 1 0
pv_phaseshift90 buf = mkOsc KR "PV_PhaseShift90" [buf] 1 0
pv_rectcomb buf teeth phase width = mkOsc KR "PV_RectComb" [buf,teeth,phase,width] 1 0
pv_rectcomb2 ba bb teeth phase width = mkOsc KR "PV_RectComb2" [ba,bb,teeth,phase,width] 1 0

pv_randcomb' id buf wp trg = mkOsc' KR "PV_RandComb" [buf,wp,trg] 1 0 id
pv_randwipe' id ba bb wp trg = mkOsc' KR "PV_RandWipe" [ba,bb,wp,trg] 1 0 id

pv_randcomb buf wp trg = uniquify (pv_randcomb' (UId 0) buf wp trg)
pv_randwipe ba bb wp trg = uniquify (pv_randwipe' (UId 0) ba bb wp trg)

-- Local Variables:
-- truncate-lines:t
-- End:
