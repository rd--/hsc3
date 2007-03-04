module Sound.SC3.Server.Graphdef (graphdef) where

import Sound.OpenSoundControl.Byte
import Sound.OpenSoundControl.Cast
import Sound.SC3.UGen.UGen (UGen(..))
import Sound.SC3.UGen.Rate (rateId)
import Sound.SC3.UGen.Graph

import Data.Word
import qualified Data.ByteString.Lazy as B

-- | Byte-encode Input value.
encode_input :: Input -> B.ByteString
encode_input (Input u p) = B.append (encode_i16 u) (encode_i16 p)

-- | Byte-encode UGen value.
encode_ugen :: Graph -> UGen -> B.ByteString
encode_ugen g c@(Control _ n _) = B.concat [B.pack (str_pstr n),
                                            encode_i16 (controlIndex g c)]
encode_ugen g (UGen r n i o s _) = B.concat [B.pack (str_pstr n),
                                             encode_i8 (rateId r),
                                             encode_i16 (length i),
                                             encode_i16 (length o),
                                             encode_i16 s,
                                             B.concat (map (encode_input . makeInput g) i),
                                             B.concat (map (encode_i8 . rateId) o)]
encode_ugen _ _  = error "illegal input"

-- | Value of Constant.
constantValue :: UGen -> Double
constantValue (Constant n) = n
constantValue  _           = error "constantValue: non Constant input"

-- | Default value of Control.
controlDefault :: UGen -> Double
controlDefault (Control _ _ n) = n
controlDefault  _              = error "controlDefault: non Control input"

-- | Construct instrument definition bytecode.
encode_graphdef :: String -> Graph -> B.ByteString
encode_graphdef s g@(Graph n c u) = B.concat [
                                     encode_str "SCgf",
                                     encode_i32 0,
                                     encode_i16 1,
                                     B.pack (str_pstr s),
                                     encode_i16 (length n),
                                     B.concat (map (encode_f32 . constantValue) n),
                                     encode_i16 (length c),
                                     B.concat (map (encode_f32 . controlDefault) c),
                                     encode_i16 (length c),
                                     B.concat (map (encode_ugen g) c),
                                     encode_i16 (length u),
                                     B.concat (map (encode_ugen g) u)]

-- | Construct instrument definition bytecode.
graphdef :: String -> Graph -> [Word8]
graphdef s g = B.unpack (encode_graphdef s g)
