import Control.Monad {- base -}
import Data.Bits {- base -}
import Data.List {- base -}
import System.Random {- random -}
import Sound.SC3 {- hsc3 -}
import qualified Sound.SC3.Common.Buffer.Gen as Gen {- hsc3 -}
import qualified Sound.SC3.Common.Math.Filter.BEQ {- hsc3 -}
import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}
import qualified Sound.SC3.UGen.Bindings.Composite.External as X {- hsc3 -}
import qualified Sound.SC3.UGen.Bindings.HW.External.F0 as X {- hsc3 -}
import qualified Sound.SC3.UGen.Bindings.HW.External.SC3_Plugins as X {- hsc3 -}
import qualified Sound.SC3.UGen.Bindings.HW.External.Zita as X {- hsc3 -}
import qualified Sound.SC3.UGen.Bindings.DB.RDU as X {- sc3-rdu -}
import qualified Sound.SC3.UGen.Protect as Protect {- hsc3-rw -}
