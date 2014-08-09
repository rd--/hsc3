-- | Zita UGen definitions.
--
-- To build the SC3 plugin run @faust2supercollider -d@ on
-- @zita_rev1.dsp@, which is in the @examples@ directory of Faust, see
-- <http://faust.grame.fr/>.
module Sound.SC3.UGen.Bindings.HW.External.Zita where

import Sound.SC3.UGen.Bindings.HW.Construct
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.Type

data ZitaRev1 a =
    ZitaRev1 {zr1_in1 :: a
             ,zr1_in2 :: a
             ,zr1_delay :: a
             ,zr1_xover :: a
             ,zr1_rtlow :: a
             ,zr1_rtmid :: a
             ,zr1_fdamp :: a
             ,zr1_eq1fr :: a
             ,zr1_eq1gn :: a
             ,zr1_eq2fr :: a
             ,zr1_eq2gn :: a
             ,zr1_opmix :: a -- ^ (-1,+1)
             ,zr1_level :: a}

zitaRev1_r :: ZitaRev1 UGen -> UGen
zitaRev1_r r =
    let (ZitaRev1 in1 in2 dly xov rtl rtm fda e1f e1g e2f e2g opm lvl) = r
    in zitaRev1 in1 in2 dly xov rtl rtm fda e1f e1g e2f e2g opm lvl

zitaRev1 :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
zitaRev1 in1 in2 dly xov rtl rtm fda e1f e1g e2f e2g opm lvl = mkFilterR [AR] "FaustZitaRev1" [in1,in2,dly,xov,rtl,rtm,fda,e1f,e1g,e2f,e2g,opm,lvl] 2

{-
hsc3-db:

std_I :: Int -> String -> Double -> I
std_I ix nm df = I (ix,ix) nm df Nothing

zitaRev1_dsc :: U
zitaRev1_dsc =
    let i = [std_I 0 "in1" 0.0
            ,std_I 1 "in2" 0.0
            ,std_I 2 "delay" 0.04
            ,std_I 3 "xover" 200.0
            ,std_I 4 "rtlow" 3.0
            ,std_I 5 "rtmid" 2.0
            ,std_I 6 "fdamp" 6.0e3
            ,std_I 7 "eq1fr" 160
            ,std_I 8 "eq1gn" 0.0
            ,std_I 9 "eq2fr" 2.5e3
            ,std_I 10 "eq2gn" 0.0
            ,std_I 11 "opmix" 0.5
            ,std_I 12 "level" (-20)]
    in U "FaustZitaRev1" [AR] AR Nothing i Nothing (Left 2) "Zita Reverb 1"
-}

