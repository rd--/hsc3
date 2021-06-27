import Prelude {- base -}
import Control.Monad {- base -}
import Data.Bits {- base -}
import Data.Function {- base -}
import Data.List {- base -}
import System.Random {- random -}
import Sound.OSC {- hosc -}
import Sound.SC3 {- hsc3 -}
import qualified Sound.SC3.Common.Base as Sound.SC3.Common.Base {- hsc3 -}
import qualified Sound.SC3.Common.Buffer.Gen as Gen {- hsc3 -}
import qualified Sound.SC3.Common.Math.Filter.BEQ as Sound.SC3.Common.Math.Filter.BEQ {- hsc3 -}
import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}
import qualified Sound.SC3.UGen.Bindings.Composite.External as X {- hsc3 -}
import qualified Sound.SC3.UGen.Bindings.HW.External.F0 as X {- hsc3 -}
import qualified Sound.SC3.UGen.Bindings.HW.External.SC3_Plugins as X {- hsc3 -}
import qualified Sound.SC3.UGen.Bindings.HW.External.Zita as X {- hsc3 -}
import qualified Sound.SC3.UGen.Bindings.DB.RDU as X {- sc3-rdu -}
import Sound.SC3.UGen.Unsafe {- hsc3-unsafe -}
import qualified Sound.SC3.UGen.Protect as Protect {- hsc3-rw -}
import qualified Sound.SC3.UI.HTML as UI {- hsc3-ui -}
import qualified Sound.SC3.UI.Plot as UI {- hsc3-ui -}
import qualified Sound.SC3.UI.Qarma as UI {- hsc3-ui -}
import qualified Sound.SC3.UI.SCLang as UI {- hsc3-ui -}
import qualified Sound.SC3.UI.SCLang.Control as UI {- hsc3-ui -}
