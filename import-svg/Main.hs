module Main where

import Data.Maybe(mapMaybe)
import Text.XML.Light

main :: IO ()
main =
  do txt <- getContents
     putStrLn $ concatMap showContent $ map filterContent $ parseXML txt

filterContent :: Content -> Content
filterContent cont =
  case cont of
    Elem el -> Elem $ add_attr a el { elContent = map simplePath (elContent el) }
      where a = Attr (blank_name { qName = "id" }) "it"
    _ -> cont

simplePath :: Content -> Content
simplePath cont =
  case cont of
    Elem el | qName (elName el) == "path" ->
                Elem el { elAttribs = mapMaybe onlyD (elAttribs el) }
            | otherwise -> Elem el { elContent = map simplePath (elContent el) }
    _ -> cont

onlyD :: Attr -> Maybe Attr
onlyD a = if qName (attrKey a) == "d" then Just a else Nothing
