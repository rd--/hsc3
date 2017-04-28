-- | Request and display status information from the synthesis server.
module Sound.SC3.Server.Status where

import qualified Data.ByteString.Char8 as C {- bytestring -}
import Data.List {- base -}
import Data.Maybe {- base -}
import qualified Data.Tree as T {- containers -}
import Sound.OSC.Type {- hosc -}

-- | Get /n/th field of status as 'Floating'.
extractStatusField :: Floating n => Int -> [Datum] -> n
extractStatusField n =
    fromMaybe (error "extractStatusField")
    . datum_floating
    . (!! n)

-- | Names of status fields.
statusFields :: [String]
statusFields =
    ["Unused                      "
    ,"# UGens                     "
    ,"# Synths                    "
    ,"# Groups                    "
    ,"# Instruments               "
    ,"% CPU (Average)             "
    ,"% CPU (Peak)                "
    ,"Sample Rate (Nominal)       "
    ,"Sample Rate (Actual)        "]

-- | Status pretty printer.
statusFormat :: [Datum] -> [String]
statusFormat d =
    let s = "***** SuperCollider Server Status *****"
    in s : zipWith (++) (tail statusFields) (map (datum_pp_typed (Just 5)) (tail d))

-- * Query Group

-- | Name or index and value or bus mapping.
type Query_Ctl = (Either String Int,Either Double Int)

-- | Nodes are either groups of synths.
data Query_Node = Query_Group Int [Query_Node]
                | Query_Synth Int String (Maybe [Query_Ctl])
                deriving (Eq,Show)

query_ctl_pp :: Query_Ctl -> String
query_ctl_pp (p,q) = either id show p ++ ":" ++ either show show q

query_node_pp :: Query_Node -> String
query_node_pp n =
    case n of
      Query_Group k _ -> show k
      Query_Synth k nm c ->
          let c' = unwords (map query_ctl_pp (fromMaybe [] c))
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
                ASCII_String nm -> Left (C.unpack nm)
                Int32 ix -> Right (fromIntegral ix)
                _ -> err "string/int32" d
        g d = case d of
                Float k -> Left (realToFrac k)
                ASCII_String b -> case C.unpack b of
                                    'c' : n -> Right (read n)
                                    _ -> err "c:_" d
                _ -> err "float/string" d
    in (f p,g q)

{- | If /rc/ is 'True' then 'Query_Ctl' data is expected (ie. flag was set at @/g_queryTree@).
/k/ is the synth-id, and /nm/ the name.

> let d = [int32 1,string "freq",float 440]
> in queryTree_synth True 1000 "saw" d

-}
queryTree_synth :: Bool -> Int -> String -> [Datum] -> (Query_Node,[Datum])
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

queryTree_group :: Bool -> Int -> Int -> [Datum] -> (Query_Node,[Datum])
queryTree_group rc gid nc =
    let recur n r d =
            if n == 0
            then (Query_Group gid (reverse r),d)
            else let (c,d') = queryTree_child rc d
                 in recur (n - 1) (c : r) d'
    in recur nc []

queryTree_child :: Bool -> [Datum] -> (Query_Node,[Datum])
queryTree_child rc d =
    case d of
      Int32 nid : Int32 (-1) : ASCII_String nm : d' ->
          queryTree_synth rc (fromIntegral nid) (C.unpack nm) d'
      Int32 gid : Int32 nc : d' ->
          queryTree_group rc (fromIntegral gid) (fromIntegral nc) d'
      _ -> error "queryTree_child"

-- | Parse result of 'g_queryTree'.
--
-- > let r = [int32 1,int32 0,int32 2,int32 1,int32 1
-- >         ,int32 100,int32 1
-- >         ,int32 1000,int32 (-1),string "saw"
-- >         ,int32 1,string "freq",float 440.0
-- >         ,int32 2,int32 0]
-- > in queryTree r
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

-- | Extact sequence of group-ids from 'Query_Node'.
queryNode_to_group_seq :: Query_Node -> [Int]
queryNode_to_group_seq nd =
    case nd of
      Query_Group k ch -> k : concatMap queryNode_to_group_seq ch
      Query_Synth _ _ _ -> []

-- | Transform 'Query_Node' to 'T.Tree'.
--
-- > putStrLn (T.drawTree (fmap query_node_pp (queryTree_rt (queryTree r))))
-- > > 0
-- > > |
-- > > +- 1
-- > > |  |
-- > > |  `- 100
-- > > |     |
-- > > |     `- (1000,"saw","freq:440.0")
-- > > |
-- > > `- 2
queryTree_rt :: Query_Node -> T.Tree Query_Node
queryTree_rt n =
    case n of
      Query_Synth _ _ _ -> T.Node n []
      Query_Group _ c -> T.Node n (map queryTree_rt c)
