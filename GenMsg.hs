-- Generates message types and parsing and printing functions from ccnx.dtd.
module Main where

import Text.XML.HaXml

main :: IO ()
main = readFile "ccnx.dtd" >>= gen . dtdParse "ccnx.dtd"
 
gen :: Maybe DocTypeDecl -> IO ()
gen Nothing = error "ccnx.dtd didn't parse"
gen (Just (DTD _ _ decls)) = putStrLn $ unlines [ code name spec | Element (ElementDecl name spec) <- decls ]

code :: String -> ContentSpec -> String
code name spec = name ++ "  " ++ case spec of
  EMPTY -> "EMPTY"
  ANY   -> "ANY"
  Mixed PCDATA -> "PCDATA"
  Mixed (PCDATAplus names) -> "PCDATA " ++ show names
  ContentSpec a -> show a

