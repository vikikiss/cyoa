{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Engine  

import Control.Monad.Writer
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

parsePage :: UNode String -> Page
parsePage node = Page (getId node) (map parseItem (filter isElement $ getChildren node))

parseItem :: UNode String -> PageItem
parseItem (Text "\n") = TextLit " "
parseItem (Text t) = TextLit $ trim t                        
parseItem node@(Element "p" _ _) = Paragraph (map parseItem $ getChildren node)
parseItem node@(Element "goto" _ _) = Goto False (read (fromJust $ getAttribute node "ref"))
parseItem node@(Element "if" _ _) =
  let cond:thn:elss = filter isElement $ getChildren node
  in If (parseCond cond) (parseBranch thn) $ case elss of
                                               [els] -> parseBranch els
                                               _ -> []
  where parseBranch node@(Element "text" _ _) = map parseItem $ getChildren node
        parseBranch node = [parseItem node]
parseItem node@(Element "inc" [("counter", counter)] _) = Inc counter                           

parseCond :: UNode String -> Cond
parseCond node@(Element "or" _ _) =
  let [left, right] = filter isElement $ getChildren node
  in parseCond left :||: parseCond right
parseCond node@(Element "eq" _ _) =
  let [left, right] = filter isElement $ getChildren node
  in parseExpr left :==: parseExpr right  

parseExpr :: UNode String -> Expr
parseExpr (Element "intlit" [("value", v)] _) = ELiteral $ read v
parseExpr (Element "counter" [("name", counter)] _) = CounterRef counter
                                                      
getId :: UNode String -> Int
getId e@(Element "page" _ _) = case getAttribute e "id" of
  Just x -> read x

trim = f . f
  where f = reverse . dropWhile isSpace

process :: String -> IO ()
process filename = do
     inputText <- L.readFile filename
     let (xml, mErr) = parse defaultParseOptions inputText
     case mErr of
       Nothing -> do         
         let page = parsePage $ (filter isElement $ getChildren xml)!!0
         doTest page
       Just err -> do
         hPutStrLn stderr $ "XML parse failed: "++show err
         exitWith $ ExitFailure 2

doTest page = do
  output <- runCyoa $ do
                -- modifyCounter (+2) "gyuru"
                execWriterT $ evalPage page
  mapM_ printOutputItem output

printOutputItem (OutText s) = putStr s
printOutputItem (OutLink n s) = putStr s
printOutputItem (OutBreak) = putStrLn ""                                
        
main = do
  args <- getArgs
  case args of
    [filename] -> process filename
    otherwise  -> do
      hPutStrLn stderr "Usage: helloworld <file.xml>"
      exitWith $ ExitFailure 1

