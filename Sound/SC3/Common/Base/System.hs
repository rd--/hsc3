module Sound.SC3.Common.Base.System where

import Control.Exception {- base -}
import Data.Maybe {- base -}
import System.Environment {- base -}

{- | 'getEnv' with error handler to return default value.  This almost works in hugs (IOException should be Exception, the signature can be elided)

> get_env_with_default "undefined_environment_variable" "default"  -- > "default"
-}
get_env_with_default :: String -> String -> IO String
get_env_with_default variableName defaultValue =
  Control.Exception.catch (getEnv variableName) ((\_ -> return defaultValue) :: IOException -> IO String)

{- | 'getEnvironment' with lookup and default value.

> get_env_default "PATH" "/usr/bin"
-}
get_env_default :: String -> String -> IO String
get_env_default variableName defaultValue = do
  env <- getEnvironment
  return (fromMaybe defaultValue (lookup variableName env))

{- | 'lookupEnv' with default value.

> lookup_env_default "PATH" "/usr/bin"
-}
lookup_env_default :: String -> String -> IO String
lookup_env_default e k = fmap (fromMaybe k) (lookupEnv e)
