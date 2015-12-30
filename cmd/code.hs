import Data.List {- base -}
import Data.List.Ordered {- data-ordlist -}
import System.Environment {- base -}
import Text.Regex {- regex-compat -}

import qualified Music.Theory.Function as T {- hmt -}

is_import :: String -> Bool
is_import = isPrefixOf "import"

has_comment :: String -> Bool
has_comment = T.predicate_all (map isInfixOf ["{- "," -}"])

import_pkg_regex :: Regex
import_pkg_regex = mkRegex "\\{- ([-a-zA-Z0-9]*) -\\}"

import_pkg :: String -> String
import_pkg s =
    case matchRegex import_pkg_regex s of
      Just [r] -> r
      _ -> error ("import_pkg: " ++ s)

-- > hs <- readFile "HS.hs"
-- > hs_pkg_dep hs
hs_pkg_dep :: String -> [String]
hs_pkg_dep s =
    let c = filter (T.predicate_and is_import has_comment) (lines s)
    in nubSort (map import_pkg c)

hs_file_pkg_dep :: FilePath -> IO [String]
hs_file_pkg_dep = fmap hs_pkg_dep . readFile

hs_file_set_pkg_dep :: [FilePath] -> IO [String]
hs_file_set_pkg_dep sq = do
  r <- mapM hs_file_pkg_dep sq
  return (nubSort (concat r))

main :: IO ()
main = do
  a <- getArgs
  case a of
    "pkg-dep":nm -> hs_file_set_pkg_dep nm >>= putStrLn . unwords
    _ -> putStrLn "code pkg-dep hs-file..."

