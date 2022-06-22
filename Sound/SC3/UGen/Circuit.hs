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
instance Reify.MuRef UGen where
  type DeRef UGen = Circuit
  mapDeRef f (UGen c) = Traversable.traverse f c
-}

ugenCircuit :: UGen -> Circuit UGen
ugenCircuit (UGen c) = c

data UGen = UGen (Circuit UGen) deriving (Eq, Read, Show)

parse_constant :: String -> Maybe UGen
parse_constant = fmap constant . Math.parse_double

u_constant :: UGen -> Maybe Double
u_constant u =
    case u of
      UGen (CConstant c) -> Just (constantValue c)
      _ -> Nothing

mrg :: [UGen] -> UGen
mrg u =
    case u of
      [] -> error "mrg: []"
      [x] -> x
      (x:xs) -> UGen (CMrg (Mrg x (mrg xs)) (rateOf x))

mrg_leftmost :: UGen -> UGen
mrg_leftmost u =
  case u of
    UGen (CMrg m _) -> mrg_leftmost (mrgLeft m)
    _ -> u

isConstant :: UGen -> Bool
isConstant = isJust . u_constant

isSink :: UGen -> Bool
isSink u =
    case mrg_leftmost u of
      UGen (CPrimitive p) -> null (ugenOutputs p)
      UGen (CMce m _) -> all isSink (mce_to_list m)
      _ -> False

un_proxy :: UGen -> Maybe (Proxy UGen)
un_proxy u =
    case u of
      UGen (CProxy p) -> Just p
      _ -> Nothing

isProxy :: UGen -> Bool
isProxy = isJust . un_proxy

mce :: [UGen] -> UGen
mce xs =
    case xs of
      [] -> error "mce: []"
      [x] -> UGen (CMce (Mce_Scalar x) (rateOf x))
      _ -> UGen (CMce (mce_from_list xs) (maximum (map rateOf xs)))

mceProxies :: Mce UGen -> [UGen]
mceProxies = mce_to_list

isMce :: UGen -> Bool
isMce u =
    case mrg_leftmost u of
      UGen (CMce _ _) -> True
      _ -> False

circuitChannels :: Circuit UGen -> [UGen]
circuitChannels u =
    case u of
      CMce m _ -> mce_to_list m
      CMrg (Mrg x y) rt -> let r:rs = mceChannels x in UGen (CMrg (Mrg r y) rt) : rs
      _ -> [UGen u]

mceChannels :: UGen -> [UGen]
mceChannels (UGen c) = circuitChannels c

mceDegree :: UGen -> Maybe Int
mceDegree u =
    case mrg_leftmost u of
      UGen (CMce m _) -> Just (length (mceProxies m))
      _ -> Nothing

mceDegree_err :: UGen -> Int
mceDegree_err = fromMaybe (error "mceDegree: not mce") . mceDegree

mceExtend :: Int -> UGen -> [UGen]
mceExtend n u =
    case u of
      UGen (CMce m _) -> mceProxies (mce_extend n m)
      UGen (CMrg (Mrg x y) rt) -> let (r:rs) = mceExtend n x in UGen (CMrg (Mrg r y) rt) : rs
      _ -> replicate n u

mceRequired :: [UGen] -> Bool
mceRequired = any isMce

mceInputTransform :: [UGen] -> Maybe [[UGen]]
mceInputTransform i =
    if mceRequired i
    then let n = maximum (map mceDegree_err (filter isMce i))
         in Just (transpose (map (mceExtend n) i))
    else Nothing

mceBuild :: ([UGen] -> UGen) -> [UGen] -> UGen
mceBuild f i =
    case mceInputTransform i of
      Nothing -> f i
      Just i' -> let xs = map (mceBuild f) i' in UGen (CMce (mce_from_list xs) (maximum (map rateOf xs)))

bracketUGen :: UGen -> Brackets -> UGen
bracketUGen u (pre, post) =
  let err = error "bracketUGen: only Constants or Primitive UGens or immediate proxies may have brackets"
      rw_proxy pxy =
        case pxy of
          UGen (CProxy (Proxy (UGen (CPrimitive p)) pix prt)) ->
            UGen (CProxy (Proxy (bracketUGen (UGen (CPrimitive p)) (pre, post)) pix prt))
          _ -> err
  in case u of
       UGen (CConstant c) -> let (lhs, rhs) = constantBrackets c in UGen (CConstant (c {constantBrackets = (lhs ++ pre, rhs ++ post)}))
       UGen (CControl c) -> let (lhs, rhs) = controlBrackets c in UGen (CControl (c {controlBrackets = (lhs ++ pre, rhs ++ post)}))
       UGen (CPrimitive p) -> let (lhs, rhs) = primitiveBrackets p in UGen (CPrimitive (p {primitiveBrackets = (lhs ++ pre, rhs ++ post)}))
       UGen (CMce m rt) ->
         if mce_is_direct_proxy m
         then UGen (CMce (mce_map rw_proxy m) rt)
         else err
       _ -> err

ugenBrackets :: UGen -> Brackets
ugenBrackets u =
  case u of
    UGen (CConstant c) -> constantBrackets c
    UGen (CControl c) -> controlBrackets c
    UGen (CPrimitive p) -> primitiveBrackets p
    _ -> emptyBrackets

checkInput :: UGen -> UGen
checkInput u =
    if isSink u
    then error ("checkInput: input is sink: " ++ show u)
    else u

constant :: Real n => n -> UGen
constant = UGen . CConstant . flip Constant emptyBrackets . realToFrac

proxy :: UGen -> Int -> UGen
proxy u n =
    case u of
      UGen (CPrimitive p) -> UGen (CProxy (Proxy (UGen (CPrimitive p)) n (ugenRate p)))
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

rateOf :: UGen -> Rate
rateOf (UGen c) = circuitRateOf c

proxify :: UGen -> UGen
proxify u =
    case u of
      UGen (CMce m _) -> mce (map proxify (mce_to_list m))
      UGen (CMrg m _) -> mrg [proxify (mrgLeft m), mrgRight m]
      UGen (CPrimitive p) ->
          let o = ugenOutputs p
          in case o of
               _:_:_ -> mce (map (proxy u) [0 .. length o - 1])
               _ -> u
      UGen (CConstant _) -> u
      _ -> error "proxify: illegal ugen"

mk_ugen_select_rate :: String -> [UGen] -> [Rate] -> Either Rate [Int] -> Rate
mk_ugen_select_rate nm h rs r =
  let r' = either id (maximum . map (rateOf . Safe.atNote ("mkUGen: " ++ nm) h)) r
  in if isRight r && r' == DemandRate && DemandRate `notElem` rs
     then if ControlRate `elem` rs then ControlRate else error "mkUGen: DemandRate input to non-ControlRate filter"
     else if r' `elem` rs || r' == DemandRate
          then r'
          else error ("mkUGen: rate restricted: " ++ show (r,r',rs,nm))

mkUGen :: Maybe ([Sample] -> Sample) -> [Rate] -> Either Rate [Int] ->
          String -> [UGen] -> Maybe [UGen] -> Int -> Special -> UGenId -> UGen
mkUGen cf rs r nm i i_mce o s z =
    let i' = maybe i ((i ++) . concatMap mceChannels) i_mce
        f h = let r' = mk_ugen_select_rate nm h rs r
                  o' = replicate o r'
                  u = UGen (CPrimitive (Primitive r' nm h o' s z ([], [])))
              in case cf of
                   Just cf' ->
                     if all isConstant h
                     then constant (cf' (mapMaybe u_constant h))
                     else u
                   Nothing -> u
    in proxify (mceBuild f (map checkInput i'))

mkOperator :: ([Sample] -> Sample) -> String -> [UGen] -> Int -> UGen
mkOperator f c i s =
    let ix = [0 .. length i - 1]
    in mkUGen (Just f) all_rates (Right ix) c i Nothing 1 (Special s) NoId

mkUnaryOperator :: SC3_Unary_Op -> (Sample -> Sample) -> UGen -> UGen
mkUnaryOperator i f a =
    let g [x] = f x
        g _ = error "mkUnaryOperator: non unary input"
    in mkOperator g "UnaryOpUGen" [a] (fromEnum i)

mkBinaryOperator_optimise_constants :: SC3_Binary_Op -> (Sample -> Sample -> Sample) ->
                                       (Either Sample Sample -> Bool) ->
                                       UGen -> UGen -> UGen
mkBinaryOperator_optimise_constants i f o a b =
   let g [x,y] = f x y
       g _ = error "mkBinaryOperator: non binary input"
       r = case (a,b) of
             (UGen (CConstant (Constant a' ([],[]))),_) ->
                 if o (Left a') then Just b else Nothing
             (_,UGen (CConstant (Constant b' ([],[])))) ->
                 if o (Right b') then Just a else Nothing
             _ -> Nothing
   in fromMaybe (mkOperator g "BinaryOpUGen" [a, b] (fromEnum i)) r

mkBinaryOperator :: SC3_Binary_Op -> (Sample -> Sample -> Sample) -> UGen -> UGen -> UGen
mkBinaryOperator i f a b =
   let g [x,y] = f x y
       g _ = error "mkBinaryOperator: non binary input"
   in mkOperator g "BinaryOpUGen" [a, b] (fromEnum i)

is_math_binop :: Int -> UGen -> Bool
is_math_binop k u =
    case u of
      UGen (CPrimitive (Primitive _ "BinaryOpUGen" [_,_] [_] (Special s) NoId _)) -> s == k
      _ -> False

is_add_operator :: UGen -> Bool
is_add_operator = is_math_binop 0

assert_is_add_operator :: String -> UGen -> UGen
assert_is_add_operator msg u = if is_add_operator u then u else error ("assert_is_add_operator: " ++ msg)

is_mul_operator :: UGen -> Bool
is_mul_operator = is_math_binop 2

mul_add_optimise_direct :: UGen -> UGen
mul_add_optimise_direct u =
  let reorder (i,j,k) =
        let (ri,rj,rk) = (rateOf i,rateOf j,rateOf k)
        in if rk > max ri rj
           then Nothing
           else Just (max (max ri rj) rk,if rj > ri then (j,i,k) else (i,j,k))
  in case assert_is_add_operator "MUL-ADD" u of
       UGen (CPrimitive (Primitive _ _ [UGen (CPrimitive (Primitive _ "BinaryOpUGen" [i,j] [_] (Special 2) NoId ([],[]))),k] [_] _ NoId ([],[]))) ->
         case reorder (i,j,k) of
           Just (rt,(p,q,r)) -> UGen (CPrimitive (Primitive rt "MulAdd" [p,q,r] [rt] (Special 0) NoId ([],[])))
           Nothing -> u
       UGen (CPrimitive (Primitive _ _ [k,UGen (CPrimitive (Primitive _ "BinaryOpUGen" [i,j] [_] (Special 2) NoId ([],[])))] [_] _ NoId ([],[]))) ->
         case reorder (i,j,k) of
           Just (rt,(p,q,r)) -> UGen (CPrimitive (Primitive rt "MulAdd" [p,q,r] [rt] (Special 0) NoId ([],[])))
           Nothing -> u
       _ -> u

mul_add_optimise :: UGen -> UGen
mul_add_optimise u = if is_add_operator u then mul_add_optimise_direct u else u

sum3_optimise_direct :: UGen -> UGen
sum3_optimise_direct u =
  case assert_is_add_operator "SUM3" u of
    UGen (CPrimitive (Primitive r _ [UGen (CPrimitive (Primitive _ "BinaryOpUGen" [i,j] [_] (Special 0) NoId ([],[]))),k] [_] _ NoId ([],[]))) ->
      UGen (CPrimitive (Primitive r "Sum3" [i,j,k] [r] (Special 0) NoId ([],[])))
    UGen (CPrimitive (Primitive r _ [k,UGen (CPrimitive (Primitive _ "BinaryOpUGen" [i,j] [_] (Special 0) NoId ([],[])))] [_] _ NoId ([],[]))) ->
      UGen (CPrimitive (Primitive r "Sum3" [i,j,k] [r] (Special 0) NoId ([],[])))
    _ -> u

sum3_optimise :: UGen -> UGen
sum3_optimise u = if is_add_operator u then sum3_optimise_direct u else u

add_optimise :: UGen -> UGen
add_optimise = sum3_optimise . mul_add_optimise

ugen_concise_pp :: UGen -> String
ugen_concise_pp u =
    let bracketed (l,r) x = l : x ++ [r]
        prim_pp (Primitive _ nm _ _ sp _ _) = ugen_user_name nm sp
        k = 5
    in case u of
         UGen (CConstant (Constant n _)) -> real_pp k n
         UGen (CControl (Control _ _ nm def _ _ _)) -> nm ++ "=" ++ real_pp k def
         UGen (CLabel (Label s)) -> bracketed ('"','"') s
         UGen (CPrimitive p) -> prim_pp p
         UGen (CProxy (Proxy p n _)) -> ugen_concise_pp p ++ "@" ++ show n
         UGen (CMce (Mce_Scalar s) _) -> ugen_concise_pp s
         UGen (CMce (Mce_Vector v) _) -> bracketed ('[',']') (intercalate "," (map (ugen_concise_pp . mce_scalar_value) v))
         UGen (CMrg (Mrg l r) _) -> unwords [ugen_concise_pp l,"&",ugen_concise_pp r]
