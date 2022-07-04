-- | Collection of modules that can be loaded into hugs.
module Sound.Sc3.Hugs (module M) where

import Sound.Sc3.Common.Base as M
import Sound.Sc3.Common.Buffer as M
import Sound.Sc3.Common.Buffer.Gen as M
import Sound.Sc3.Common.Context as M
import Sound.Sc3.Common.Enum as M
import Sound.Sc3.Common.Envelope as M
import Sound.Sc3.Common.Math as M
import Sound.Sc3.Common.Math.Filter as M
import Sound.Sc3.Common.Math.Filter.Beq as M
import Sound.Sc3.Common.Math.Noise as M
import Sound.Sc3.Common.Math.Operator as M
import Sound.Sc3.Common.Math.Warp as M
import Sound.Sc3.Common.Math.Window as M
import Sound.Sc3.Common.Mce as M
import Sound.Sc3.Common.Monad as M
import Sound.Sc3.Common.Monad.Operators as M
import Sound.Sc3.Common.Random as M
import Sound.Sc3.Common.Rate as M
import Sound.Sc3.Common.Unsafe as M

{-
import Sound.Sc3.Common.Base.System as M
import Sound.Sc3.Common.Buffer.Array as M
import Sound.Sc3.Common.Buffer.Vector as M
import Sound.Sc3.Common.Help as M
import Sound.Sc3.Common.Math.Interpolate as M
import Sound.Sc3.Common.SoundFile as M
import Sound.Sc3.Common.Uid as M
-}

import Sound.Sc3.Ugen.Brackets as M
import Sound.Sc3.Ugen.Constant as M
import Sound.Sc3.Ugen.Control as M
import Sound.Sc3.Ugen.Hs as M
import Sound.Sc3.Ugen.Label as M
import Sound.Sc3.Ugen.Mrg as M
import Sound.Sc3.Ugen.Name as M
import Sound.Sc3.Ugen.Plain as M
import Sound.Sc3.Ugen.Pp as M
import Sound.Sc3.Ugen.Primitive as M
import Sound.Sc3.Ugen.Proxy as M
import Sound.Sc3.Ugen.Ugen as M

{-
import Sound.Sc3.Ugen.Analysis as M
-}

import Sound.Sc3.Server.Enum as M
import Sound.Sc3.Server.Options as M
import Sound.Sc3.Server.Param as M

{-
import Sound.Sc3.Server.Command as M
import Sound.Sc3.Server.Command.Plain as M
import Sound.Sc3.Server.Graphdef.Text as M
import Sound.Sc3.Server.Nrt as M
-}
