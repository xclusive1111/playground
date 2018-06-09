module PutJSON where

import Data.List (intercalate)
import SimpleJSON

renderJValue :: JValue -> String
renderJValue (JString s)   = show s
renderJValue (JNumber n)   = show n
renderJValue (JBool True)  = show "true"
renderJValue (JBool False) = show "false"
renderJValue JNull         = "null"

renderJValue (JObject o)  = "{" ++ pairs o ++ "}"
  where pairs [] = ""
        pairs ps = intercalate ", " (map doPair ps)
        doPair (k, v)  = show k ++ ":" ++ renderJValue v

renderJValue (JArray a) = "[" ++ renderArr a  ++"]"
  where renderArr [] = ""
        renderArr arr = intercalate ", " (map renderJValue arr)

putJValue :: JValue -> IO ()
putJValue v = putStrLn (renderJValue v)
