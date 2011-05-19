{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, OverloadedStrings #-}
-- module Cyoa.Web.Web where

import Cyoa.Monad  
import Cyoa.Parser
import Cyoa.PageLang
import Cyoa.Engine
  
import Yesod
import Web.Routes.Quasi.Classes
import Control.Applicative ((<$>), (<*>))
import Data.Maybe
import Data.Text
import Control.Monad
import Data.IORef
import System.Environment (getArgs)
import System.Exit
import System.IO
import System.IO.Unsafe
import Control.Monad.RWS
  
import qualified Text.Blaze.Html4.Strict as HTML  
import Text.Hamlet
  
import Data.Set (Set)
import qualified Data.Set as Set
  
data CyoaWeb = CyoaWeb
               
type Handler = GHandler CyoaWeb CyoaWeb

mkYesod "CyoaWeb" [$parseRoutes|                     
/ PRoot GET
/start PStart GET
/goto/#Link PGoto GET       
|]  

instance Yesod CyoaWeb where
    approot _ = ""
    -- clientSessionDuration _ = 60                

instance SinglePiece Link where
  toSinglePiece = pack . show
  fromSinglePiece s = case reads $ unpack s of
                        ((link, _):_) -> Just link
                        [] -> Nothing
  

toHamlet :: Output -> Hamlet (Route CyoaWeb)
toHamlet (OutputClear title items) = [$hamlet|
                                      <h1>#{title}
                                      $forall item <- items
                                        ^{itemToHamlet item}               
                                     |]
toHamlet (OutputContinue items) = [$hamlet|
                                   $forall item <- items
                                     ^{itemToHamlet item}               
                                  |]

itemToHamlet :: OutputItem -> Hamlet (Route CyoaWeb)
itemToHamlet (OutText _ s) = [$hamlet|#{s}|]
itemToHamlet (OutBreak) = [$hamlet|<br>|]
itemToHamlet (OutDie n) = [$hamlet|[#{n}]|] -- TODO
itemToHamlet (OutLink link s) = [$hamlet|<a href="@{PGoto link}">#{s}|]
itemToHamlet (OutEnemies e) = [$hamlet| |]
                              
                
getState :: Handler GameState
getState = do
  state <- lookupSession "state"
  case state of
    Nothing -> redirect RedirectTemporary PStart
    Just state -> return $ unserialize state

setState :: GameState -> Handler ()
setState = setSession "state" . serialize 

refPages :: IORef [Page]
refPages = unsafePerformIO $ newIORef (error "refPages")

stepEngine :: CyoaT IO a -> Handler (Either GameEvent a, Output)
stepEngine f = do
  s <- getState
  pages <- liftIO $ readIORef refPages
  (x, s', w) <- liftIO $ stepCyoa f pages s
  setState s'
  return (x, w)
  
           
getPRoot :: Handler RepHtml
getPRoot = do
  (result, output) <- stepEngine evalPage
  defaultLayout $ addHamlet $ toHamlet output
           
getPStart :: Handler ()
getPStart = do
  state <- liftIO $ mkGameState
  setState state
  redirect RedirectTemporary PRoot

getPGoto :: Link -> Handler ()  
getPGoto link = do
  stepEngine (goto link)
  redirect RedirectTemporary PRoot

serialize :: GameState -> Text
serialize = pack . show           
           
unserialize = read . unpack
               
main = do
  args <- getArgs
  pages <- case args of
    [filename] -> parsePages filename
    otherwise  -> do
      hPutStrLn stderr "Usage: cyoa-web <file.xml>"
      exitWith $ ExitFailure 1
  writeIORef refPages pages

  warpDebug 3000 CyoaWeb
           
