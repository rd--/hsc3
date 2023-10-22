{- | Request and display status information from the synthesis server.

\/status messages receive \/status.reply messages.

\/g_queryTree messages recieve \/g_queryTree.reply messages.

-}
module Sound.Sc3.Server.Status where

import Data.List {- base -}
import Data.Maybe {- base -}
import Text.Printf {- base -}

import qualified Data.ByteString.Char8 as C {- bytestring -}
import qualified Data.Tree as T {- containers -}
import qualified Safe {- safe -}

import Sound.Osc.Datum {- hosc -}
import Sound.Osc.Text {- hosc -}

import Sound.Sc3.Server.Command.Plain

-- * Status

-- | Get /n/th field of /status.reply message as 'Floating'.
extractStatusField :: Floating n => Int -> [Datum] -> n
extractStatusField n =
    fromMaybe (error "extractStatusField")
    . datum_floating
    . flip (Safe.atNote "extractStatusField") n

-- | Names of /status.reply fields sent in reply to /status request.
statusFields :: [String]
statusFields =
    ["Unused                      "
    ,"# Ugens                     "
    ,"# Synths                    "
    ,"# Groups                    "
    ,"# Synthdefs                 "
    ,"% CPU (Average)             "
    ,"% CPU (Peak)                "
    ,"Sample Rate (Nominal)       "
    ,"Sample Rate (Actual)        "]

-- | Status pretty printer.
statusFormat :: [Datum] -> [String]
statusFormat d =
    let s = "***** SuperCollider Server Status *****"
        t = Safe.tailNote "statusFormat"
    in s : zipWith (++) (t statusFields) (map (showDatum (Just 5)) (t d))

-- | Concise pretty printer, one line, omits Peak-Cpu and Nominal-Sr.
status_format_concise :: [Datum] -> String
status_format_concise d =
  case d of
    [Int32 _,Int32 ugn,Int32 grp,Int32 syn,Int32 ins,Float cpu1,Float _cpu2,Double _sr1,Double sr2] ->
      printf
      "UGN=%-5d GRP=%-5d SYN=%-5d INS=%-5d CPU=%-5.1f SR=%-7.1f"
      ugn grp syn ins cpu1 sr2
    _ -> error "status_format_concise?"

-- * Query Group

-- | Name or index and value or bus mapping.
type Query_Ctl = (Either String Int,Either Double Int)

-- | Nodes are either groups of synths.
data Query_Node = Query_Group Group_Id [Query_Node]
                | Query_Synth Synth_Id String (Maybe [Query_Ctl])
                deriving (Eq,Show)

-- | Pretty-print 'Query_Ctl'
query_ctl_pp :: Query_Ctl -> String
query_ctl_pp (p,q) = either id show p ++ ":" ++ either show show q

-- | Pretty-print 'Query_Node'
query_node_pp :: Query_Node -> String
query_node_pp n =
    case n of
      Query_Group k _ -> show k
      Query_Synth k nm c ->
          let c' = unwords (maybe [] (map query_ctl_pp) c)
          in show (k,nm,c')

-- | Control (parameter) data may be given as names or indices and as
-- values or bus mappings.
--
-- > queryTree_ctl (string "freq",float 440) == (Left "freq",Left 440.0)
-- > queryTree_ctl (int32 1,string "c0") == (Right 1,Right 0)
queryTree_ctl :: (Datum,Datum) -> Query_Ctl
queryTree_ctl (p,q) =
    let err msg val = error (show ("queryTree_ctl",msg,val))
        f d = case d of
                AsciiString nm -> Left (C.unpack nm)
                Int32 ix -> Right (fromIntegral ix)
                _ -> err "string/int32" d
        g d = case d of
                Float k -> Left (realToFrac k)
                AsciiString b -> case C.unpack b of
                                    'c' : n -> Right (read n)
                                    _ -> err "c:_" d
                _ -> err "float/string" d
    in (f p,g q)

{- | If /rc/ is 'True' then 'Query_Ctl' data is expected (ie. flag was set at @\/g_queryTree@).
/k/ is the synth-id, and /nm/ the name.

> let d = [int32 1,string "freq",float 440]
> in queryTree_synth True 1000 "saw" d

-}
queryTree_synth :: Bool -> Synth_Id -> String -> [Datum] -> (Query_Node,[Datum])
queryTree_synth rc k nm d =
    let pairs l = case l of
                    e0:e1:l' -> (e0,e1) : pairs l'
                    _ -> []
        f r = case r of
                Int32 n : r' -> let (p,r'') = genericSplitAt (n * 2) r'
                                in (map queryTree_ctl (pairs p),r'')
                _ -> error "queryTree_synth"
    in if rc
       then let (p,d') = f d
            in (Query_Synth k nm (Just p),d')
       else (Query_Synth k nm Nothing,d)

-- | Generate 'Query_Node' for indicated 'Group_Id'.
queryTree_group :: Bool -> Group_Id -> Int -> [Datum] -> (Query_Node,[Datum])
queryTree_group rc gid nc =
    let recur n r d =
            if n == 0
            then (Query_Group gid (reverse r),d)
            else let (c,d') = queryTree_child rc d
                 in recur (n - 1) (c : r) d'
    in recur nc []

-- | Either 'queryTree_synth' or 'queryTree_group'.
queryTree_child :: Bool -> [Datum] -> (Query_Node,[Datum])
queryTree_child rc d =
    case d of
      Int32 nid : Int32 (-1) : AsciiString nm : d' ->
          queryTree_synth rc (fromIntegral nid) (C.unpack nm) d'
      Int32 gid : Int32 nc : d' ->
          queryTree_group rc (fromIntegral gid) (fromIntegral nc) d'
      _ -> error "queryTree_child"

-- | Parse result of ' g_queryTree '.
queryTree :: [Datum] -> Query_Node
queryTree d =
    case d of
      Int32 rc : Int32 gid : Int32 nc : d' ->
          let rc' = rc /= 0
              gid' = fromIntegral gid
              nc' = fromIntegral nc
          in case queryTree_group rc' gid' nc' d' of
               (r,[]) -> r
               _ -> error "queryTree"
      _ -> error "queryTree"

-- | Extact sequence of 'Group_Id's from 'Query_Node'.
queryNode_to_group_seq :: Query_Node -> [Group_Id]
queryNode_to_group_seq nd =
    case nd of
      Query_Group k ch -> k : concatMap queryNode_to_group_seq ch
      Query_Synth _ _ _ -> []

-- | Transform 'Query_Node' to 'T.Tree'.
queryTree_rt :: Query_Node -> T.Tree Query_Node
queryTree_rt n =
    case n of
      Query_Synth _ _ _ -> T.Node n []
      Query_Group _ c -> T.Node n (map queryTree_rt c)
