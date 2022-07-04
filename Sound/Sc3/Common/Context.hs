-- | Interpreter (module) contexts for hsc3.
module Sound.Sc3.Common.Context where

import Text.Printf {- base -}

-- | (moduleName, qualifierName, packageName)
type Context = [(String, Maybe String, String)]

-- | Format a Context as a sequence of import commands.
context_format :: Context -> [String]
context_format =
  let f (moduleName,qualifier,packageName) =
        case qualifier of
          Nothing -> printf "import %s {- %s -}" moduleName packageName
          Just qualifierName -> printf "import qualified %s as %s {- %s -}" moduleName qualifierName packageName
  in map f

-- | writeFile of context_format
context_write :: FilePath -> Context -> IO ()
context_write fn = writeFile fn . unlines . context_format

-- | Minimal hsc3 context
min_context :: Context
min_context =
  [("Prelude",Nothing,"base")
  ,("Control.Monad",Nothing,"base")
  ,("Data.Bits",Nothing,"base")
  ,("Data.Function",Nothing,"base")
  ,("Data.List",Nothing,"base")
  ,("Sound.Sc3",Nothing,"hsc3")]

-- | Standard hsc3 context
std_context :: Context
std_context =
  [("Prelude",Nothing,"base")
  ,("Control.Monad",Nothing,"base")
  ,("Data.Bits",Nothing,"base")
  ,("Data.Function",Nothing,"base")
  ,("Data.List",Nothing,"base")
  ,("System.Random",Nothing,"random")
  ,("Sound.Osc",Nothing,"hosc")
  ,("Sound.Sc3",Nothing,"hsc3")
  ,("Sound.Sc3.Common.Base",Just "Sound.Sc3.Common.Base","hsc3")
  ,("Sound.Sc3.Common.Buffer.Gen",Just "Gen","hsc3")
  ,("Sound.Sc3.Common.Math.Filter.Beq",Just "Sound.Sc3.Common.Math.Filter.Beq","hsc3")
  ,("Sound.Sc3.Ugen.Bindings.Db.External",Just "X","hsc3")
  ,("Sound.Sc3.Ugen.Bindings.Composite.External",Just "X","hsc3")
  ,("Sound.Sc3.Ugen.Bindings.Hw.External.F0",Just "X","hsc3")
  ,("Sound.Sc3.Ugen.Bindings.Hw.External.SC3_Plugins",Just "X","hsc3")
  ,("Sound.Sc3.Ugen.Bindings.Hw.External.Zita",Just "X","hsc3")
  ,("Sound.Sc3.Ugen.Bindings.Db.Rdu",Just "X","sc3-rdu")
  ,("Sound.Sc3.Ugen.Dot",Just "Sound.Sc3.Ugen.Dot","hsc3-dot")
  ,("Sound.Sc3.Ugen.Unsafe",Nothing,"hsc3-unsafe")
  ,("Sound.Sc3.Ugen.Unsafe",Nothing,"hsc3-unsafe")
  ,("Sound.Sc3.Ugen.Protect",Just "Protect","hsc3-rw")
  ,("Sound.Sc3.Ui.Html",Just "Ui","hsc3-ui")
  ,("Sound.Sc3.Ui.Plot",Just "Ui","hsc3-ui")
  ,("Sound.Sc3.Ui.Qarma",Just "Ui","hsc3-ui")
  ,("Sound.Sc3.Ui.ScLang",Just "Ui","hsc3-ui")
  ,("Sound.Sc3.Ui.ScLang.Control",Just "Ui","hsc3-ui")]
