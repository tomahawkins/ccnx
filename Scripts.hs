-- Generates message types and parsing and printing functions from ccnx.dtd.
module Main where

import Data.List
import Text.XML.HaXml

main :: IO ()
main = readFile "ccnx.dtd" >>= gen . dtdParse "ccnx.dtd"
 
gen :: Maybe DocTypeDecl -> IO ()
gen Nothing = error "ccnx.dtd didn't parse"
gen (Just (DTD _ _ decls)) = putStrLn $ unlines $ map code elements
  where
  elements = required [ a | Element a <- decls ]

required :: [ElementDecl] -> [ElementDecl]
required elements = map element $ accumulate [] ["ContentObject", "Interest"]
  where

  accumulate :: [Name] -> [Name] -> [Name]
  accumulate registered needed
    | null needed = registered
    | otherwise = accumulate (sort $ diff ++ registered) $ concat [ dependents a | a <- diff ]
    where
    diff = nub $ needed \\ registered

  dependents :: Name -> [Name]
  dependents name = case element name of
    ElementDecl _ (ContentSpec a) -> nub $ names a
    _ -> []

  names :: CP -> [Name]
  names (TagName a _) = [a]
  names (Choice  a _) = concatMap names a
  names (Seq     a _) = concatMap names a
  
  element :: Name -> ElementDecl
  element name = case [ a | a@(ElementDecl n _) <- elements, n == name ] of
    [a] -> a
    _ -> error $ "unknown, or duplicate element: " ++ name

code :: ElementDecl -> String
code (ElementDecl name spec) = name ++ "  " ++ case spec of
  EMPTY -> "EMPTY"
  ANY   -> "ANY"
  Mixed PCDATA -> "PCDATA"
  Mixed (PCDATAplus names) -> "PCDATA " ++ show names
  ContentSpec a -> show a

