module Hsc.PV where

import Hsc.UGen

pv_add ba bb = UGen KR "PV_Add" [ba,bb] [KR] 0 r0
pv_binscramble buf wp width trg = UGen KR "PV_BinScramble" [buf,wp,width,trg] [KR] 0 r0
pv_binshift buf str shift = UGen KR "PV_BinShift" [buf,str,shift] [KR] 0 r0
pv_binwipe ba bb wp = UGen KR "PV_BinWipe" [ba,bb,wp] [KR] 0 r0
pv_brickwall buf wp = UGen KR "PV_BrickWall" [buf,wp] [KR] 0 r0
pv_conformalmap buf real imag = UGen KR "PV_ConformalMap" [buf,real,imag] [KR] 0 r0
pv_copyphase ba bb = UGen KR "PV_CopyPhase" [ba,bb] [KR] 0 r0
pv_diffuser buf trg = UGen KR "PV_Diffuser" [buf,trg] [KR] 0 r0
pv_hainsworthfoote buf h f thr wait = UGen KR "PV_HainsworthFoote" [buf,h,f,thr,wait] [KR] 0 r0
pv_jensenandersen buf sc hfe hfc sf thr wait = UGen KR "PV_JensenAndersen" [buf,sc,hfe,hfc,sf,thr,wait] [KR] 0 r0
pv_localmax buf thr = UGen KR "PV_LocalMax" [buf,thr] [KR] 0 r0
pv_magabove buf thr = UGen KR "PV_MagAbove" [buf,thr] [KR] 0 r0
pv_magbelow buf thr = UGen KR "PV_MagBelow" [buf,thr] [KR] 0 r0
pv_magclip buf thr = UGen KR "PV_MagClip" [buf,thr] [KR] 0 r0
pv_magfreeze buf frz = UGen KR "PV_MagFreeze" [buf,frz] [KR] 0 r0
pv_magmul ba bb = UGen KR "PV_MagMul" [ba,bb] [KR] 0 r0
pv_magnoise buf = UGen KR "PV_MagNoise" [buf] [KR] 0 r0
pv_magshift buf str shift = UGen KR "PV_MagShift" [buf,str,shift] [KR] 0 r0
pv_magsmear buf bins = UGen KR "PV_MagSmear" [buf,bins] [KR] 0 r0
pv_magsquared buf = UGen KR "PV_MagSquared" [buf] [KR] 0 r0
pv_max ba bb = UGen KR "PV_Max" [ba,bb] [KR] 0 r0
pv_min ba bb = UGen KR "PV_Min" [ba,bb] [KR] 0 r0
pv_mul ba bb = UGen KR "PV_Mul" [ba,bb] [KR] 0 r0
pv_phaseshift buf shift = UGen KR "PV_PhaseShift" [buf,shift] [KR] 0 r0
pv_phaseshift270 buf = UGen KR "PV_PhaseShift270" [buf] [KR] 0 r0
pv_phaseshift90 buf = UGen KR "PV_PhaseShift90" [buf] [KR] 0 r0
pv_randcomb id buf wp trg = UGen KR "PV_RandComb" [buf,wp,trg] [KR] 0 id
pv_randwipe id ba bb wp trg = UGen KR "PV_RandWipe" [ba,bb,wp,trg] [KR] 0 id
pv_rectcomb buf teeth phase width = UGen KR "PV_RectComb" [buf,teeth,phase,width] [KR] 0 r0
pv_rectcomb2 ba bb teeth phase width = UGen KR "PV_RectComb2" [ba,bb,teeth,phase,width] [KR] 0 r0

-- Local Variables:
-- truncate-lines:t
-- End:
