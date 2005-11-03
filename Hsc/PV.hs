module Hsc.PV where

import Hsc.UGen

pv_add r ba bb = UGen r "PV_Add" [ba,bb] [r] 0 0
pv_binscramble r buf wp width trg = UGen r "PV_BinScramble" [buf,wp,width,trg] [r] 0 0
pv_binshift r buf str shift = UGen r "PV_BinShift" [buf,str,shift] [r] 0 0
pv_binwipe r ba bb wp = UGen r "PV_BinWipe" [ba,bb,wp] [r] 0 0
pv_brickwall r buf wp = UGen r "PV_BrickWall" [buf,wp] [r] 0 0
pv_conformalmap r buf real imag = UGen r "PV_ConformalMap" [buf,real,imag] [r] 0 0
pv_copyphase r ba bb = UGen r "PV_CopyPhase" [ba,bb] [r] 0 0
pv_diffuser r buf trg = UGen r "PV_Diffuser" [buf,trg] [r] 0 0
pv_hainsworthfoote r buf h f thr wait = UGen r "PV_HainsworthFoote" [buf,h,f,thr,wait] [r] 0 0
pv_jensenandersen r buf sc hfe hfc sf thr wait = UGen r "PV_JensenAndersen" [buf,sc,hfe,hfc,sf,thr,wait] [r] 0 0
pv_localmax r buf thr = UGen r "PV_LocalMax" [buf,thr] [r] 0 0
pv_magabove r buf thr = UGen r "PV_MagAbove" [buf,thr] [r] 0 0
pv_magbelow r buf thr = UGen r "PV_MagBelow" [buf,thr] [r] 0 0
pv_magclip r buf thr = UGen r "PV_MagClip" [buf,thr] [r] 0 0
pv_magfreeze r buf frz = UGen r "PV_MagFreeze" [buf,frz] [r] 0 0
pv_magmul r ba bb = UGen r "PV_MagMul" [ba,bb] [r] 0 0
pv_magnoise r buf = UGen r "PV_MagNoise" [buf] [r] 0 0
pv_magshift r buf str shift = UGen r "PV_MagShift" [buf,str,shift] [r] 0 0
pv_magsmear r buf bins = UGen r "PV_MagSmear" [buf,bins] [r] 0 0
pv_magsquared r buf = UGen r "PV_MagSquared" [buf] [r] 0 0
pv_max r ba bb = UGen r "PV_Max" [ba,bb] [r] 0 0
pv_min r ba bb = UGen r "PV_Min" [ba,bb] [r] 0 0
pv_mul r ba bb = UGen r "PV_Mul" [ba,bb] [r] 0 0
pv_phaseshift r buf shift = UGen r "PV_PhaseShift" [buf,shift] [r] 0 0
pv_phaseshift270 r buf = UGen r "PV_PhaseShift270" [buf] [r] 0 0
pv_phaseshift90 r buf = UGen r "PV_PhaseShift90" [buf] [r] 0 0
pv_randcomb id r buf wp trg = UGen r "PV_RandComb" [buf,wp,trg] [r] 0 id
pv_randwipe id r ba bb wp trg = UGen r "PV_RandWipe" [ba,bb,wp,trg] [r] 0 id
pv_rectcomb r buf teeth phase width = UGen r "PV_RectComb" [buf,teeth,phase,width] [r] 0 0
pv_rectcomb2 r ba bb teeth phase width = UGen r "PV_RectComb2" [ba,bb,teeth,phase,width] [r] 0 0

-- Local Variables:
-- truncate-lines:t
-- End:            
