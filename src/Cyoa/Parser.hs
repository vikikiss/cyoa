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
parseItem node@(Element "goto-lucky" [("refYes", yes), ("refNo", no)] _) = GotoLucky (read yes) (read no) -- TODO
parseItem node@(Element "if" _ _) =
  let cond:thn:elss = filter isElement $ getChildren node
  in If (parseCond cond) (parseBranch thn) $ case elss of
                                               [els] -> parseBranch els
                                               _ -> []
  where parseBranch node@(Element "text" _ _) = map parseItem $ getChildren node
        parseBranch node = [parseItem node]
parseItem node@(Element "inc" [("counter", counter)] _) = Inc counter                           
parseItem node@(Element "dec" [("counter", counter)] _) = Dec counter                           
parseItem node@(Element "clear" [("counter", counter)] _) = Clear counter                           
parseItem node@(Element "take" [("item", item)] _) = Take item
parseItem node@(Element "damage" [("stat", stat)] _) = Damage (parseStat stat) (parseExpr expr)
  where [expr] = filter isElement $ getChildren node                                                     
parseItem node@(Element "heal" [("stat", stat)] _) = Heal (parseStat stat) (parseExpr expr)
  where [expr] = filter isElement $ getChildren node                                                     
parseItem node@(Element "set-flag" [("flag", flag)] _) = Set flag
parseItem node@(Element "dice" [("name", name)] _) = DieDef name
parseItem node@(Element "fight" _ _) = Fight $ map parseEnemy $ filter isElement $ getChildren node

parseEnemy node@(Element "enemy" [("agility", agility), ("health", health)] [(Text name)]) = Enemy name (read agility) (read health)
                                                     
parseStat "health" = Health
parseStat "luck" = Luck
parseStat "agility" = Agility

parseCond :: UNode String -> Cond
parseCond node@(Element "or" _ _) =
  let [left, right] = filter isElement $ getChildren node
  in parseCond left :||: parseCond right
parseCond node@(Element "eq" _ _) = parseBin node (:==:)
parseCond node@(Element "le" _ _) = parseBin node (:<=:)
parseCond node@(Element "ge" _ _) = parseBin node (:>=:)
parseCond node@(Element "lt" _ _) = parseBin node (:<:)
parseCond node@(Element "gt" _ _) = parseBin node (:>:)
parseCond node@(Element "carry" [("item", item)] _) = Carry item
parseCond node@(Element "flag-set" [("flag", flag)] _) = FlagSet flag

parseBin node op =
  let [left, right] = filter isElement $ getChildren node
  in parseExpr left `op` parseExpr right
                                                      
parseExpr :: UNode String -> Expr
parseExpr (Element "intlit" [("value", v)] _) = ELiteral $ read v
parseExpr (Element "counter" [("name", counter)] _) = CounterRef counter
parseExpr (Element "var" [("ref", name)] _) = DieRef name
parseExpr node@(Element "plus" _ _) = parseBin node (:+:)
parseExpr node@(Element "minus" _ _) = parseBin node (:-:)
parseExpr node@(Element "mul" _ _) = parseBin node (:*:)
parseExpr node@(Element "mod" _ _) = parseBin node (:%:)
parseExpr (Element "score" [("stat", stat)] _) = AbilityQuery (parseStat stat)
parseExpr node@(Element "cond" _ _) =
  let [cond, thn, els] = filter isElement $ getChildren node
  in ECond (parseCond cond) (parseExpr thn) (parseExpr els)
                                                 
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
