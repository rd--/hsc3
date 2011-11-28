-- | RDU UGen definitions.
module Sound.SC3.UGen.External.RDU where

import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.UGen

rFreezer :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
rFreezer b l r g i io ir rr ps pt nl =
    mkOsc AR "RFreezer" [b,l,r,g,i,io,ir,rr,ps,pt,nl] 1

rFreezer_dsc :: [String]
rFreezer_dsc = ["bufnum"
               ,"left"
               ,"right"
               ,"gain"
               ,"increment"
               ,"incrementOffset"
               ,"incrementRandom"
               ,"rightRandom"
               ,"syncPhaseTrigger"
               ,"randomizePhaseTrigger"
               ,"numberOfLoops"]

rShufflerB :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
rShufflerB b rlL rlR riL riR dL dR eaL eaR esL esR ekL ekR slM slR ioL ioR i riQ ioQ =
    let p = [b,rlL,rlR,riL,riR,dL,dR,eaL,eaR,esL,esR,ekL,ekR,slM,slR,ioL,ioR,i,riQ,ioQ]
    in mkOsc AR "RShufflerB" p 2

data RShufflerB a = RShufflerB {bufnum :: a
                               ,readLocationMinima :: a
                               ,readLocationMaxima :: a
                               ,readIncrementMinima :: a
                               ,readIncrementMaxima :: a
                               ,durationMinima :: a
                               ,durationMaxima :: a
                               ,envelopeAmplitudeMinima :: a
                               ,envelopeAmplitudeMaxima :: a
                               ,envelopeShapeMinima :: a
                               ,envelopeShapeMaxima :: a
                               ,envelopeSkewMinima :: a
                               ,envelopeSkewMaxima :: a
                               ,stereoLocationMinima :: a
                               ,stereoLocationMaxima :: a
                               ,interOffsetTimeMinima :: a
                               ,interOffsetTimeMaxima :: a
                               ,ftableReadLocationIncrement :: a
                               ,readIncrementQuanta :: a
                               ,interOffsetTimeQuanta :: a
                               }

rShufflerB_r :: RShufflerB UGen -> UGen
rShufflerB_r r =
    let (RShufflerB b rlL rlR riL riR dL dR eaL eaR esL esR ekL ekR slM slR ioL ioR i riQ ioQ) = r
    in rShufflerB b rlL rlR riL riR dL dR eaL eaR esL esR ekL ekR slM slR ioL ioR i riQ ioQ

rShufflerL_dsc :: [String]
rShufflerL_dsc = ["in","fragmentSize","maxDelay"]

rShufflerL :: UGen -> UGen -> UGen -> UGen
rShufflerL i fs md = mkFilterR [AR] "RShufflerL" [i,fs,md] 1

-- Local Variables:
-- truncate-lines:t
-- End:
