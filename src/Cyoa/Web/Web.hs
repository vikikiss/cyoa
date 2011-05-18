{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, OverloadedStrings #-}
module Cyoa.Web.Web where

import Cyoa.Monad  
import Cyoa.Parser
import Cyoa.PageLang
  
import Yesod
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
  
import Data.Set (Set)
import qualified Data.Set as Set
  
data CyoaWeb = CyoaWeb
               
type Handler = GHandler CyoaWeb CyoaWeb

mkYesod "CyoaWeb" [$parseRoutes|                     
/ Root GET
/start Start GET
|]  

getState :: Handler GameState
getState = do
  state <- lookupSession "state"
  case state of
    Nothing -> redirect RedirectTemporary Start
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
  
           
getRoot :: Handler RepHtml
getRoot = do
  (Right pagenum, _) <- stepEngine (gets $ player_page . player_state)
  defaultLayout [$hamlet|<h1>#{pagenum}|]
  -- state <- getState
  -- case (page state) of
  --   1 -> do
  --     defaultLayout $ do
  --       when ("kard" `Set.member` (items state)) $
  --         addHtml $ HTML.p $ HTML.toHtml ("Van egy fasza kardod" :: String)
  --       addHamlet [$hamlet|
  --         <h1>#{page state}              
  --         <p><a href=@{Goto 2}>Menj a masodik oldalra
  --         |]
  --   2 -> do
  --     setState state{ items = Set.insert "kard" $ items state }
  --     defaultLayout [$hamlet|
  --                    <h1>#{page state}              
  --                    <p><a href=@{Goto 1}>Menj az elso oldalra
  --                    |]           
           
getStart :: Handler ()
getStart = do
  state <- liftIO $ mkGameState
  setState state
  redirect RedirectTemporary Root

-- getGoto :: Int -> Handler ()  
-- getGoto n = do
--   state <- getState
--   setState $ state{ page = n }
--   redirect RedirectTemporary Root

serialize :: GameState -> Text
serialize = pack . show           
           
unserialize = read . unpack
               
instance Yesod CyoaWeb where
    approot _ = ""
    -- clientSessionDuration _ = 60
                
main = do
  args <- getArgs
  pages <- case args of
    [filename] -> parsePages filename
    otherwise  -> do
      hPutStrLn stderr "Usage: cyoa-web <file.xml>"
      exitWith $ ExitFailure 1
  writeIORef refPages pages

  warpDebug 3000 CyoaWeb
           
