{-# LANGUAGE ScopedTypeVariables #-}
module Cyoa.Parser (parsePages) where

import Cyoa.NormalizeXML
import Cyoa.Engine  

import Control.Monad.Writer
import Text.XML.Expat.Tree 
import Text.XML.Expat.Format
import System.Exit
import System.IO
import Data.List
import Data.Maybe
import qualified Data.ByteString.Lazy as L
import Control.Monad
import Data.Char

parsePage :: UNode String -> Page
parsePage node = Page (getId node) (map parseItem (filter isElement $ getChildren node))

parseItem :: UNode String -> PageItem
parseItem (Text t) = TextLit t                        
parseItem node@(Element "p" _ _) = Paragraph (map parseItem $ getChildren node)
parseItem node@(Element "goto" [("ref", pageNum)] _) = Goto False (read pageNum)
parseItem node@(Element "Goto" [("ref", pageNum)] _) = Goto True (read pageNum)
parseItem node@(Element "if" _ _) =
  let cond:thn:elss = filter isElement $ getChildren node
  in If (parseCond cond) (parseBranch thn) $ case elss of
                                               [els] -> parseBranch els
                                               _ -> []
  where parseBranch node@(Element "text" _ _) = map parseItem $ getChildren node
        parseBranch node = [parseItem node]
parseItem node@(Element "inc" [("counter", counter)] _) = Inc counter                           
parseItem node@(Element "take" [("item", item)] _) = Take item

parseCond :: UNode String -> Cond
parseCond node@(Element "or" _ _) =
  let [left, right] = filter isElement $ getChildren node
  in parseCond left :||: parseCond right
parseCond node@(Element "eq" _ _) =
  let [left, right] = filter isElement $ getChildren node
  in parseExpr left :==: parseExpr right  
parseCond node@(Element "carry" [("item", item)] _) = Carry item

parseExpr :: UNode String -> Expr
parseExpr (Element "intlit" [("value", v)] _) = ELiteral $ read v
parseExpr (Element "counter" [("name", counter)] _) = CounterRef counter
                                                      
getId :: UNode String -> Int
getId e@(Element "page" _ _) = case getAttribute e "id" of
  Just x -> read x

parsePages :: FilePath -> IO [Page]
parsePages filename = do
     inputText <- L.readFile filename
     let (xml, mErr) = parse defaultParseOptions inputText
     case mErr of
       Nothing -> do         
         return $ map (parsePage . normalizeXML) (filter isElement $ getChildren xml)
       Just err -> do
         hPutStrLn stderr $ "XML parse failed: " ++ show err
         exitWith $ ExitFailure 2
