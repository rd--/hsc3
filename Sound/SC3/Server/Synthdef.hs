-- | The unit-generator graph structure implemented by the
--   SuperCollider synthesis server.
module Sound.SC3.Server.Synthdef where

import qualified Data.ByteString.Lazy as B {- bytestring -}
import Data.List
import Sound.OpenSoundControl.Coding.Byte {- hosc -}
import Sound.OpenSoundControl.Coding.Cast
import Sound.SC3.Server.Synthdef.Internal
import Sound.SC3.Server.Synthdef.Type
import Sound.SC3.UGen.UGen
import System.FilePath {- filepath -}

-- | Transform a unit generator into a graph.
--
-- > import Sound.SC3.UGen
-- > synth (out 0 (pan2 (sinOsc AR 440 0) 0.5 0.1))
synth :: UGen -> Graph
synth u =
    let (_,g) = mk_node (prepare_root u) empty_graph
        g' = g {ugens = reverse (ugens g)}
    in add_implicit g'

-- | Transform a unit generator graph into bytecode.
graphdef :: Graph -> Graphdef
graphdef = encode_graphdef

-- | Encode 'Synthdef' as a binary data stream.
synthdefData :: Synthdef -> Graphdef
synthdefData (Synthdef s g) =
    B.concat [encode_str "SCgf"
             ,encode_i32 0
             ,encode_i16 1
             ,B.pack (str_pstr s)
             ,encode_graphdef g]

-- | Transform a unit generator synth definition into bytecode.
synthdef :: String -> UGen -> Synthdef
synthdef s u = Synthdef s (synth u)

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
        f g = let h (x:xs) = (x,length (x:xs))
                  h [] = error "synthstat"
              in show . map h . group . sort . map g
    in unlines ["number of constants       : " ++ show (length cs)
               ,"number of controls        : " ++ show (length ks)
               ,"control rates             : " ++ f node_k_rate ks
               ,"number of unit generators : " ++ show (length us)
               ,"unit generator rates      : " ++ f node_u_rate us]


