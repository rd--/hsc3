-- | Interpreter (module) contexts for hsc3.
module Sound.SC3.Common.Context where

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
  ,("Sound.SC3",Nothing,"hsc3")]

-- | Standard hsc3 context
std_context :: Context
std_context =
  [("Prelude",Nothing,"base")
  ,("Control.Monad",Nothing,"base")
  ,("Data.Bits",Nothing,"base")
  ,("Data.Function",Nothing,"base")
  ,("Data.List",Nothing,"base")
  ,("System.Random",Nothing,"random")
  ,("Sound.OSC",Nothing,"hosc")
  ,("Sound.SC3",Nothing,"hsc3")
  ,("Sound.SC3.Common.Base",Just "Sound.SC3.Common.Base","hsc3")
  ,("Sound.SC3.Common.Buffer.Gen",Just "Gen","hsc3")
  ,("Sound.SC3.Common.Math.Filter.BEQ",Just "Sound.SC3.Common.Math.Filter.BEQ","hsc3")
  ,("Sound.SC3.UGen.Bindings.DB.External",Just "X","hsc3")
  ,("Sound.SC3.UGen.Bindings.Composite.External",Just "X","hsc3")
  ,("Sound.SC3.UGen.Bindings.HW.External.F0",Just "X","hsc3")
  ,("Sound.SC3.UGen.Bindings.HW.External.SC3_Plugins",Just "X","hsc3")
  ,("Sound.SC3.UGen.Bindings.HW.External.Zita",Just "X","hsc3")
  ,("Sound.SC3.UGen.Bindings.DB.RDU",Just "X","sc3-rdu")
  ,("Sound.SC3.UGen.Dot",Just "Sound.SC3.UGen.Dot","hsc3-dot")
  ,("Sound.SC3.UGen.Unsafe",Nothing,"hsc3-unsafe")
  ,("Sound.SC3.UGen.Unsafe",Nothing,"hsc3-unsafe")
  ,("Sound.SC3.UGen.Protect",Just "Protect","hsc3-rw")
  ,("Sound.SC3.UI.HTML",Just "UI","hsc3-ui")
  ,("Sound.SC3.UI.Plot",Just "UI","hsc3-ui")
  ,("Sound.SC3.UI.Qarma",Just "UI","hsc3-ui")
  ,("Sound.SC3.UI.SCLang",Just "UI","hsc3-ui")
  ,("Sound.SC3.UI.SCLang.Control",Just "UI","hsc3-ui")]
