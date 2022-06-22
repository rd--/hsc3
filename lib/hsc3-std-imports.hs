import Prelude {- base -}
import Control.Monad {- base -}
import Data.Bits {- base -}
import Data.Function {- base -}
import Data.List {- base -}
import System.Random {- random -}
import Sound.Osc {- hosc -}
import Sound.Sc3 {- hsc3 -}
import Sound.Sc3.Common.Base {- hsc3 -}
import qualified Sound.Sc3.Common.Buffer.Gen as Gen {- hsc3 -}
import qualified Sound.Sc3.Common.Math.Filter.Beq {- hsc3 -}
import qualified Sound.Sc3.Ugen.Bindings.Db.External as X {- hsc3 -}
import qualified Sound.Sc3.Ugen.Bindings.Composite.External as X {- hsc3 -}
import qualified Sound.Sc3.Ugen.Bindings.Hw.External.Sc3_Plugins as X {- hsc3 -}
import qualified Sound.Sc3.Ugen.Bindings.Hw.External.Zita as X {- hsc3 -}
import qualified Sound.Sc3.Ugen.Protect as Protect {- hsc3-rw -}
import qualified Sound.Sc3.Ui.Html as Ui {- hsc3-ui -}
import qualified Sound.Sc3.Ui.Plot as Ui {- hsc3-ui -}
import qualified Sound.Sc3.Ui.Qarma as Ui {- hsc3-ui -}
import qualified Sound.Sc3.Ui.ScLang as Ui {- hsc3-ui -}
import qualified Sound.Sc3.Ui.ScLang.Control as Ui {- hsc3-ui -}
