{- | Circuit
This model requires caching rate values at Proxy and Mce and Mrg.
This makes rate-rewriting code more intricate
-}
data Circuit t
  = CConstant Constant
  | CControl Control
  | CLabel Label
  | CPrimitive (Primitive t)
  | CProxy (Proxy t) -- ^ Output port at multi-channel primitive
  | CMce (Mce t) Rate -- ^ Multiple channel expansion
  | CMrg (Mrg t) Rate -- ^ Multiple root graph
  deriving (Ord, Eq, Read, Show)

{-
{-# Language TypeFamilies #-}
{-# Language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
import qualified Data.Traversable as Traversable {- containers -}
import qualified Data.Reify as Reify {- data-reify -}
instance Reify.MuRef Ugen where
  type DeRef Ugen = Circuit
  mapDeRef f (Ugen c) = Traversable.traverse f c
-}

ugenCircuit :: Ugen -> Circuit Ugen
ugenCircuit (Ugen c) = c

data Ugen = Ugen (Circuit Ugen) deriving (Eq, Read, Show)

parse_constant :: String -> Maybe Ugen
parse_constant = fmap constant . Math.parse_double

u_constant :: Ugen -> Maybe Double
u_constant u =
    case u of
      Ugen (CConstant c) -> Just (constantValue c)
      _ -> Nothing

mrg :: [Ugen] -> Ugen
mrg u =
    case u of
      [] -> error "mrg: []"
      [x] -> x
      (x:xs) -> Ugen (CMrg (Mrg x (mrg xs)) (rateOf x))

mrg_leftmost :: Ugen -> Ugen
mrg_leftmost u =
  case u of
    Ugen (CMrg m _) -> mrg_leftmost (mrgLeft m)
    _ -> u

isConstant :: Ugen -> Bool
isConstant = isJust . u_constant

isSink :: Ugen -> Bool
isSink u =
    case mrg_leftmost u of
      Ugen (CPrimitive p) -> null (ugenOutputs p)
      Ugen (CMce m _) -> all isSink (mce_to_list m)
      _ -> False

un_proxy :: Ugen -> Maybe (Proxy Ugen)
un_proxy u =
    case u of
      Ugen (CProxy p) -> Just p
      _ -> Nothing

isProxy :: Ugen -> Bool
isProxy = isJust . un_proxy

mce :: [Ugen] -> Ugen
mce xs =
    case xs of
      [] -> error "mce: []"
      [x] -> Ugen (CMce (Mce_Scalar x) (rateOf x))
      _ -> Ugen (CMce (mce_from_list xs) (maximum (map rateOf xs)))

mceProxies :: Mce Ugen -> [Ugen]
mceProxies = mce_to_list

isMce :: Ugen -> Bool
isMce u =
    case mrg_leftmost u of
      Ugen (CMce _ _) -> True
      _ -> False

circuitChannels :: Circuit Ugen -> [Ugen]
circuitChannels u =
    case u of
      CMce m _ -> mce_to_list m
      CMrg (Mrg x y) rt -> let r:rs = mceChannels x in Ugen (CMrg (Mrg r y) rt) : rs
      _ -> [Ugen u]

mceChannels :: Ugen -> [Ugen]
mceChannels (Ugen c) = circuitChannels c

mceDegree :: Ugen -> Maybe Int
mceDegree u =
    case mrg_leftmost u of
      Ugen (CMce m _) -> Just (length (mceProxies m))
      _ -> Nothing

mceDegree_err :: Ugen -> Int
mceDegree_err = fromMaybe (error "mceDegree: not mce") . mceDegree

mceExtend :: Int -> Ugen -> [Ugen]
mceExtend n u =
    case u of
      Ugen (CMce m _) -> mceProxies (mce_extend n m)
      Ugen (CMrg (Mrg x y) rt) -> let (r:rs) = mceExtend n x in Ugen (CMrg (Mrg r y) rt) : rs
      _ -> replicate n u

mceRequired :: [Ugen] -> Bool
mceRequired = any isMce

mceInputTransform :: [Ugen] -> Maybe [[Ugen]]
mceInputTransform i =
    if mceRequired i
    then let n = maximum (map mceDegree_err (filter isMce i))
         in Just (transpose (map (mceExtend n) i))
    else Nothing

mceBuild :: ([Ugen] -> Ugen) -> [Ugen] -> Ugen
mceBuild f i =
    case mceInputTransform i of
      Nothing -> f i
      Just i' -> let xs = map (mceBuild f) i' in Ugen (CMce (mce_from_list xs) (maximum (map rateOf xs)))

bracketUgen :: Ugen -> Brackets -> Ugen
bracketUgen u (pre, post) =
  let err = error "bracketUgen: only Constants or Primitive Ugens or immediate proxies may have brackets"
      rw_proxy pxy =
        case pxy of
          Ugen (CProxy (Proxy (Ugen (CPrimitive p)) pix prt)) ->
            Ugen (CProxy (Proxy (bracketUgen (Ugen (CPrimitive p)) (pre, post)) pix prt))
          _ -> err
  in case u of
       Ugen (CConstant c) -> let (lhs, rhs) = constantBrackets c in Ugen (CConstant (c {constantBrackets = (lhs ++ pre, rhs ++ post)}))
       Ugen (CControl c) -> let (lhs, rhs) = controlBrackets c in Ugen (CControl (c {controlBrackets = (lhs ++ pre, rhs ++ post)}))
       Ugen (CPrimitive p) -> let (lhs, rhs) = primitiveBrackets p in Ugen (CPrimitive (p {primitiveBrackets = (lhs ++ pre, rhs ++ post)}))
       Ugen (CMce m rt) ->
         if mce_is_direct_proxy m
         then Ugen (CMce (mce_map rw_proxy m) rt)
         else err
       _ -> err

ugenBrackets :: Ugen -> Brackets
ugenBrackets u =
  case u of
    Ugen (CConstant c) -> constantBrackets c
    Ugen (CControl c) -> controlBrackets c
    Ugen (CPrimitive p) -> primitiveBrackets p
    _ -> emptyBrackets

checkInput :: Ugen -> Ugen
checkInput u =
    if isSink u
    then error ("checkInput: input is sink: " ++ show u)
    else u

constant :: Real n => n -> Ugen
constant = Ugen . CConstant . flip Constant emptyBrackets . realToFrac

proxy :: Ugen -> Int -> Ugen
proxy u n =
    case u of
      Ugen (CPrimitive p) -> Ugen (CProxy (Proxy (Ugen (CPrimitive p)) n (ugenRate p)))
      _ -> error "proxy: not primitive?"

circuitRateOf :: Circuit t -> Rate
circuitRateOf u =
    case u of
      CConstant _ -> InitialisationRate
      CControl c -> controlOperatingRate c
      CLabel _ -> InitialisationRate
      CPrimitive p -> ugenRate p
      CProxy p -> proxyRate p
      CMce _ rt -> rt
      CMrg _ rt -> rt

rateOf :: Ugen -> Rate
rateOf (Ugen c) = circuitRateOf c

proxify :: Ugen -> Ugen
proxify u =
    case u of
      Ugen (CMce m _) -> mce (map proxify (mce_to_list m))
      Ugen (CMrg m _) -> mrg [proxify (mrgLeft m), mrgRight m]
      Ugen (CPrimitive p) ->
          let o = ugenOutputs p
          in case o of
               _:_:_ -> mce (map (proxy u) [0 .. length o - 1])
               _ -> u
      Ugen (CConstant _) -> u
      _ -> error "proxify: illegal ugen"

mk_ugen_select_rate :: String -> [Ugen] -> [Rate] -> Either Rate [Int] -> Rate
mk_ugen_select_rate nm h rs r =
  let r' = either id (maximum . map (rateOf . Safe.atNote ("mkUgen: " ++ nm) h)) r
  in if isRight r && r' == DemandRate && DemandRate `notElem` rs
     then if ControlRate `elem` rs then ControlRate else error "mkUgen: DemandRate input to non-ControlRate filter"
     else if r' `elem` rs || r' == DemandRate
          then r'
          else error ("mkUgen: rate restricted: " ++ show (r,r',rs,nm))

mkUgen :: Maybe ([Sample] -> Sample) -> [Rate] -> Either Rate [Int] ->
          String -> [Ugen] -> Maybe [Ugen] -> Int -> Special -> UgenId -> Ugen
mkUgen cf rs r nm i i_mce o s z =
    let i' = maybe i ((i ++) . concatMap mceChannels) i_mce
        f h = let r' = mk_ugen_select_rate nm h rs r
                  o' = replicate o r'
                  u = Ugen (CPrimitive (Primitive r' nm h o' s z ([], [])))
              in case cf of
                   Just cf' ->
                     if all isConstant h
                     then constant (cf' (mapMaybe u_constant h))
                     else u
                   Nothing -> u
    in proxify (mceBuild f (map checkInput i'))

mkOperator :: ([Sample] -> Sample) -> String -> [Ugen] -> Int -> Ugen
mkOperator f c i s =
    let ix = [0 .. length i - 1]
    in mkUgen (Just f) all_rates (Right ix) c i Nothing 1 (Special s) NoId

mkUnaryOperator :: SC3_Unary_Op -> (Sample -> Sample) -> Ugen -> Ugen
mkUnaryOperator i f a =
    let g [x] = f x
        g _ = error "mkUnaryOperator: non unary input"
    in mkOperator g "UnaryOpUgen" [a] (fromEnum i)

mkBinaryOperator_optimise_constants :: SC3_Binary_Op -> (Sample -> Sample -> Sample) ->
                                       (Either Sample Sample -> Bool) ->
                                       Ugen -> Ugen -> Ugen
mkBinaryOperator_optimise_constants i f o a b =
   let g [x,y] = f x y
       g _ = error "mkBinaryOperator: non binary input"
       r = case (a,b) of
             (Ugen (CConstant (Constant a' ([],[]))),_) ->
                 if o (Left a') then Just b else Nothing
             (_,Ugen (CConstant (Constant b' ([],[])))) ->
                 if o (Right b') then Just a else Nothing
             _ -> Nothing
   in fromMaybe (mkOperator g "BinaryOpUgen" [a, b] (fromEnum i)) r

mkBinaryOperator :: SC3_Binary_Op -> (Sample -> Sample -> Sample) -> Ugen -> Ugen -> Ugen
mkBinaryOperator i f a b =
   let g [x,y] = f x y
       g _ = error "mkBinaryOperator: non binary input"
   in mkOperator g "BinaryOpUgen" [a, b] (fromEnum i)

is_math_binop :: Int -> Ugen -> Bool
is_math_binop k u =
    case u of
      Ugen (CPrimitive (Primitive _ "BinaryOpUgen" [_,_] [_] (Special s) NoId _)) -> s == k
      _ -> False

is_add_operator :: Ugen -> Bool
is_add_operator = is_math_binop 0

assert_is_add_operator :: String -> Ugen -> Ugen
assert_is_add_operator msg u = if is_add_operator u then u else error ("assert_is_add_operator: " ++ msg)

is_mul_operator :: Ugen -> Bool
is_mul_operator = is_math_binop 2

mul_add_optimise_direct :: Ugen -> Ugen
mul_add_optimise_direct u =
  let reorder (i,j,k) =
        let (ri,rj,rk) = (rateOf i,rateOf j,rateOf k)
        in if rk > max ri rj
           then Nothing
           else Just (max (max ri rj) rk,if rj > ri then (j,i,k) else (i,j,k))
  in case assert_is_add_operator "MUL-ADD" u of
       Ugen (CPrimitive (Primitive _ _ [Ugen (CPrimitive (Primitive _ "BinaryOpUgen" [i,j] [_] (Special 2) NoId ([],[]))),k] [_] _ NoId ([],[]))) ->
         case reorder (i,j,k) of
           Just (rt,(p,q,r)) -> Ugen (CPrimitive (Primitive rt "MulAdd" [p,q,r] [rt] (Special 0) NoId ([],[])))
           Nothing -> u
       Ugen (CPrimitive (Primitive _ _ [k,Ugen (CPrimitive (Primitive _ "BinaryOpUgen" [i,j] [_] (Special 2) NoId ([],[])))] [_] _ NoId ([],[]))) ->
         case reorder (i,j,k) of
           Just (rt,(p,q,r)) -> Ugen (CPrimitive (Primitive rt "MulAdd" [p,q,r] [rt] (Special 0) NoId ([],[])))
           Nothing -> u
       _ -> u

mul_add_optimise :: Ugen -> Ugen
mul_add_optimise u = if is_add_operator u then mul_add_optimise_direct u else u

sum3_optimise_direct :: Ugen -> Ugen
sum3_optimise_direct u =
  case assert_is_add_operator "SUM3" u of
    Ugen (CPrimitive (Primitive r _ [Ugen (CPrimitive (Primitive _ "BinaryOpUgen" [i,j] [_] (Special 0) NoId ([],[]))),k] [_] _ NoId ([],[]))) ->
      Ugen (CPrimitive (Primitive r "Sum3" [i,j,k] [r] (Special 0) NoId ([],[])))
    Ugen (CPrimitive (Primitive r _ [k,Ugen (CPrimitive (Primitive _ "BinaryOpUgen" [i,j] [_] (Special 0) NoId ([],[])))] [_] _ NoId ([],[]))) ->
      Ugen (CPrimitive (Primitive r "Sum3" [i,j,k] [r] (Special 0) NoId ([],[])))
    _ -> u

sum3_optimise :: Ugen -> Ugen
sum3_optimise u = if is_add_operator u then sum3_optimise_direct u else u

add_optimise :: Ugen -> Ugen
add_optimise = sum3_optimise . mul_add_optimise

ugen_concise_pp :: Ugen -> String
ugen_concise_pp u =
    let bracketed (l,r) x = l : x ++ [r]
        prim_pp (Primitive _ nm _ _ sp _ _) = ugen_user_name nm sp
        k = 5
    in case u of
         Ugen (CConstant (Constant n _)) -> real_pp k n
         Ugen (CControl (Control _ _ nm def _ _ _)) -> nm ++ "=" ++ real_pp k def
         Ugen (CLabel (Label s)) -> bracketed ('"','"') s
         Ugen (CPrimitive p) -> prim_pp p
         Ugen (CProxy (Proxy p n _)) -> ugen_concise_pp p ++ "@" ++ show n
         Ugen (CMce (Mce_Scalar s) _) -> ugen_concise_pp s
         Ugen (CMce (Mce_Vector v) _) -> bracketed ('[',']') (intercalate "," (map (ugen_concise_pp . mce_scalar_value) v))
         Ugen (CMrg (Mrg l r) _) -> unwords [ugen_concise_pp l,"&",ugen_concise_pp r]
