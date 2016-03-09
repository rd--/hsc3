import Data.List {- base -}
import Data.Maybe {- base -}
import Text.ParserCombinators.Parsec {- parsec -}

data Sig a = Atom a
           | List (Sig a)
           | Seq [Sig a]
           deriving (Eq,Show)

sig_map :: (t -> a) -> Sig t -> Sig a
sig_map f s =
    case s of
      Atom a -> Atom (f a)
      List s' -> List (sig_map f s')
      Seq s' -> Seq (map (sig_map f) s')

sig_atoms :: Sig a -> [a]
sig_atoms s =
    case s of
      Atom a -> [a]
      List s' -> sig_atoms s'
      Seq s' -> concatMap sig_atoms s'

sig_atoms_set :: Ord a => Sig a -> [a]
sig_atoms_set = nub . sort . sig_atoms

type P a = GenParser Char () a

p_atom :: P (Sig Char)
p_atom = fmap Atom (oneOf "ifsbped." <?> "p_atom")

p_brackets :: P a -> P a
p_brackets p = do
  _ <- char '['
  r <- p
  _ <- char ']'
  return r

p_list :: P (Sig Char)
p_list = fmap List (p_brackets p_sig)

p_sig :: P (Sig Char)
p_sig = fmap Seq (many1 (choice [p_atom,p_list]))

parse_sig :: String -> String -> Either ParseError (Sig Char)
parse_sig = parse p_sig

-- > sig_words show (parse_sig_err "i")
-- > sig_words show (parse_sig_err "[if]")
-- > sig_words show (parse_sig_err "i[f]")
-- > sig_words show (parse_sig_err "if[s.]e")
parse_sig_err :: String -> Sig Char
parse_sig_err s =
    case parse_sig "parse_sig_err" s of
      Left err -> error (show err)
      Right r -> r

add_brackets :: String -> String
add_brackets s = concat ["[",s,"]"]

add_paren :: String -> String
add_paren s = if null s then [] else concat ["(",s,")"]

sig_words :: (a -> String) -> Sig a -> String
sig_words f =
    let go in_list s =
            case s of
              Atom c -> f c
              Seq l -> intercalate (if in_list then "," else " -> ") (map (go in_list) l)
              List (Seq [e]) -> add_brackets (go True e)
              List s' -> add_brackets (add_paren (go True s'))
    in go False

-- > s <- readFile "/home/rohan/sw/hsc3/gen/server-command.text"
-- > let c = map words (lines s)
-- > putStrLn$unlines$map mk_generic c
mk_generic :: [String] -> String
mk_generic s =
    case s of
      [] -> undefined
      [nm] -> nm ++ " :: Message"
      nm:sig:_ ->
          let sig' = parse_sig_err sig
              cls = sig_atoms_set sig'
              sig'' = sig_liftc_generic sig'
          in nm ++ " :: " ++ gen_classes cls ++ sig_words id sig'' ++ " -> Message"

-- | tag -> (class,OSC-type,HS-type)
typec :: Char -> (String,String,String)
typec c =
    case c of
      'i' -> ("Integral","Int32","Int")
      'f' -> ("Real","Float","Double")
      's' -> ("","ASCII","String")
      'b' -> ("","Blob","ByteString")
      'p' -> ("","Int32","Bool")
      'e' -> ("","","Enum")
      '.' -> ("","Datum","Datum")
      _ -> error "typec"

sig_liftc_generic :: Sig Char -> Sig String
sig_liftc_generic =
    let f c = case typec c of
                ("",_,r) -> r
                _ -> [c]
    in sig_map f

-- > gen_classes ""
-- > gen_classes "if"
gen_classes :: [Char] -> String
gen_classes s =
    let f c = case typec c of
                ("",_,_) -> Nothing
                (cl,_,_) -> Just (cl ++ " " ++ [c])
    in case intercalate "," (mapMaybe f s) of
         "" -> ""
         r -> add_paren r ++ " => "
