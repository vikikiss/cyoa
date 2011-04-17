{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Text.XML.Expat.Tree
import Text.XML.Expat.Format
import System.Environment
import System.Exit
import System.IO
import Data.List
import Data.Maybe
import qualified Data.ByteString.Lazy as L
import Control.Monad
import Data.Char
import Data.Function (on)
  
data TextFragment = Spacing String
                  | Content String
                  deriving Show
  
splitSpaces :: Node a String -> [Node a TextFragment]
splitSpaces (Element e as ns) = [Element e (map (fmap Content) as) (concatMap splitSpaces ns)]
splitSpaces (Text t) = map Text $ filter (not . isEmpty) $ [Spacing intro, Content content, Spacing outro]
  where (intro, rest) = split t
        (outro', content') = split (reverse rest)
        outro = reverse outro'
        content = reverse content'
        split = break (not . isSpace)
        isEmpty (Spacing []) = True
        isEmpty (Content []) = True
        isEmpty _ = False

normalizeSpaces :: [Node a TextFragment] -> [Node a String]
normalizeSpaces ns = concatMap toNode $ groupBy ((==) `on` isSpacing) $ trimmed
  where trimmedHead = dropWhile isSpacing ns
        trimmed = reverse $ dropWhile isSpacing $ reverse trimmedHead
        convert (Text tf) = Text $ toString tf
        convert (Element e as ns) = Element e (map (fmap toString) as) $ normalizeSpaces ns
        toString (Spacing s) = s
        toString (Content s) = s
        isSpacing (Text (Spacing _)) = True
        isSpacing _ = False

        toNode (n:ns) | isSpacing n = [Text " "]
        toNode ns = map convert ns
                  
process :: String -> IO ()
process filename = do
     inputText <- L.readFile filename
     let (xml, mErr) = parse defaultParseOptions inputText
     case mErr of
       Nothing -> do         
         let (page :: UNode String) = (filter isElement $ getChildren xml)!!0
         print $ normalizeSpaces $ splitSpaces page
       Just err -> do
         hPutStrLn stderr $ "XML parse failed: "++show err
         exitWith $ ExitFailure 2

main = do
  args <- getArgs
  case args of
    [filename] -> process filename
    otherwise  -> do
      hPutStrLn stderr "Usage: helloworld <file.xml>"
      exitWith $ ExitFailure 1

