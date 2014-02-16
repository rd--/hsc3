-- | The unit-generator graph structure implemented by the
--   SuperCollider synthesis server.
module Sound.SC3.Server.Synthdef where

import qualified Data.ByteString.Char8 as C {- bytestring -}
import qualified Data.ByteString.Lazy as B {- bytestring -}
import Data.Default {- data-default -}
import Data.List {- base -}
import Data.Maybe {- base -}
import Sound.OSC.Coding.Byte {- hosc -}
import Sound.OSC.Coding.Cast {- hosc -}
import System.FilePath {- filepath -}

import Sound.SC3.Server.Synthdef.Internal
import Sound.SC3.Server.Synthdef.Type
import Sound.SC3.UGen.Graph
import Sound.SC3.UGen.Type
import Sound.SC3.UGen.UGen

-- | Transform a unit generator into a graph.
--
-- > import Sound.SC3.UGen
-- > synth (out 0 (pan2 (sinOsc AR 440 0) 0.5 0.1))
synth :: UGen -> Graph
synth = pv_validate . mk_graph

-- | Binary representation of a unit generator synth definition.
data Synthdef = Synthdef {synthdefName :: String
                         ,synthdefGraph :: Graph}
                deriving (Eq,Show)

instance Default Synthdef where def = defaultSynthdef

-- | Lift a 'UGen' graph into a 'Synthdef'.
synthdef :: String -> UGen -> Synthdef
synthdef s u = Synthdef s (synth u)

-- | The SC3 /default/ instrument 'Synthdef', see
-- 'default_ugen_graph'.
--
-- > withSC3 (send (d_recv defaultSynthdef))
-- > audition defaultSynthdef
defaultSynthdef :: Synthdef
defaultSynthdef = synthdef "default" default_ugen_graph

-- | The SC3 /default/ sample (buffer) playback instrument 'Synthdef',
-- see 'default_sampler_ugen_graph'.
defaultSampler :: Bool -> Synthdef
defaultSampler = synthdef "default-sampler" . default_sampler_ugen_graph

-- | Parameter names at 'Synthdef'.
--
-- > synthdefParam def == ["amp","pan","gate","freq"]
synthdefParam :: Synthdef -> [String]
synthdefParam = map node_k_name . controls . synthdefGraph

-- | Transform a unit generator graph into bytecode.
graphdef :: Graph -> Graphdef
graphdef = encode_graphdef

-- | Find the indices of the named UGen at 'Graph'.  The index is
-- required when using 'Sound.SC3.Server.Command.u_cmd'.
ugenIndices :: String -> Graph -> [Integer]
ugenIndices nm =
    let f (k,nd) =
            case nd of
              NodeU _ _ nm' _ _ _ _ -> if nm == nm' then Just k else Nothing
              _ -> Nothing
    in mapMaybe f . zip [0..] . ugens

-- | Encode 'Synthdef' as a binary data stream.
synthdefData :: Synthdef -> Graphdef
synthdefData (Synthdef s g) =
    B.concat [encode_str (C.pack "SCgf")
             ,encode_i32 0
             ,encode_i16 1
             ,B.pack (str_pstr s)
             ,encode_graphdef g]

-- | Write 'Synthdef' to indicated directory.  The filename is the
-- 'synthdefName' with the appropriate extension (@scsyndef@).
synthdefWrite :: Synthdef -> FilePath -> IO ()
synthdefWrite s dir =
    let nm = dir </> synthdefName s <.> "scsyndef"
    in B.writeFile nm (synthdefData s)

-- | Simple statistical analysis of a unit generator graph.
synthstat :: UGen -> String
synthstat u =
    let s = synth u
        cs = constants s
        ks = controls s
        us = ugens s
        u_nm z = ugen_user_name (node_u_name z) (node_u_special z)
        f g = let h (x:xs) = (x,length (x:xs))
                  h [] = error "synthstat"
              in show . map h . group . sort . map g
        sq = intercalate "," (map u_nm us)
    in unlines ["number of constants       : " ++ show (length cs)
               ,"number of controls        : " ++ show (length ks)
               ,"control rates             : " ++ f node_k_rate ks
               ,"number of unit generators : " ++ show (length us)
               ,"unit generator rates      : " ++ f node_u_rate us
               ,"unit generator sequence   : " ++ sq]
