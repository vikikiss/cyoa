{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
-- module Cyoa.Web.Web where

import Cyoa.Monad
import Cyoa.Parser
import Cyoa.PageLang
import Cyoa.Engine

import Control.Monad.State

import Yesod
import Yesod.Routes.Class
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
import Control.Monad.Writer

import qualified Text.Blaze.Html4.Strict as HTML
import Text.Hamlet
import Network.HTTP.Types (temporaryRedirect307)

import Data.Set (Set)
import qualified Data.Set as Set

data CyoaWeb = CyoaWeb

type Handler_ = GHandler CyoaWeb CyoaWeb

mkYesod "CyoaWeb" [parseRoutes|
/ PRoot GET
/start PStart GET
/goto/#Link PGoto GET
|]

instance Yesod CyoaWeb where
    approot = ApprootRelative
    -- clientSessionDuration _ = 60
    defaultLayout contents = do
      PageContent title head body <- widgetToPageContent $ do
        addCassius $ [cassius|
                      .btn, .btnUsed, a
                        color: blue
                        text-decoration: underline
                      .btn
                        cursor: pointer
                     |]
        addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.5/jquery.min.js"
        addWidget contents
      hamletToRepHtml [hamlet|
                       !!!
                       <html>
                         <head>
                           <title>#{title}
                           ^{head}
                         <body>
                           ^{body}
                       |]

instance PathPiece Link where
  toPathPiece = pack . show
  fromPathPiece s = case reads $ unpack s of
                        ((link, _):_) -> Just link
                        [] -> Nothing


type LinkFactory = Route CyoaWeb -> [(Text, Text)] -> Text
type Ham = LinkFactory -> Html

toHamlet :: Output -> Ham
toHamlet (OutputClear title items) = [hamlet|
                                      <h1>#{title}
                                      ^{itemsToHamlet 0 items}
                                      |]
toHamlet (OutputContinue items) = itemsToHamlet 0 items

itemsToHamlet :: Int -> [OutputItem] -> Ham
itemsToHamlet x [] = [hamlet|
                      <br>
                      <div #cont>
                     |]
itemsToHamlet x ((OutText _ s):is) = [hamlet|
                                      #{s}
                                      ^{itemsToHamlet x is}
                                     |]
itemsToHamlet x (OutBreak:is) = [hamlet|
                                 <br>
                                 ^{itemsToHamlet x is}
                                |]
itemsToHamlet x ((OutDie n):is) = [hamlet|
                                   ^{roll}
                                   <span #hide-#{x} style="visibility:hidden">
                                     [#{n}]
                                     ^{itemsToHamlet (succ x) is}
                                  |]
  where roll = [hamlet|<a .btn #roll-#{x} onClick="$('#roll-#{x}').remove(); $('#hide-#{x}').css('visibility','visible'); $('#hide-#{x}').removeAttr('id')">Dobj!|]
itemsToHamlet x ((OutLink link s):is) = [hamlet|
                                         ^{linkToHamlet link}
                                         ^{itemsToHamlet x is}
                                        |]
  where linkToHamlet link@(PageLink _) = [hamlet|<a href="@{PGoto link}">#{s}|]
        linkToHamlet link = [hamlet|<a .btn onClick="$.get('@{PGoto link}', function(newPage){ $('.btn').each(function(){$(this).removeAttr('onClick');$(this).attr('class', 'btnUsed');}); $('#cont').replaceWith(newPage);})">#{s}|]
itemsToHamlet x ((OutEnemies e):is) = itemsToHamlet x is
itemsToHamlet x ((OutImage img):is) = itemsToHamlet x is

getState :: Handler_ GameState
getState = do
  state <- lookupSession "state"
  case state of
    Nothing -> redirectWith temporaryRedirect307 PStart
    Just state -> return $ unserialize state

setState :: GameState -> Handler_ ()
setState = setSession "state" . serialize

refPages :: IORef [Page]
refPages = unsafePerformIO $ newIORef (error "refPages")

stepEngine :: CyoaT IO a -> Handler_ (Either GameEvent a, Output)
stepEngine f = do
  s <- getState
  pages <- liftIO $ readIORef refPages
  (x, s', w) <- liftIO $ stepCyoa f pages s
  setState s'
  return (x, w)

render output@(OutputClear title _) = defaultLayout $ do
  setTitle $ HTML.string title
  addHamlet $ toHamlet output
render output@(OutputContinue _) = hamletToRepHtml $ toHamlet output

getPRoot :: Handler_ RepHtml
getPRoot = do
  (result, output) <- stepEngine evalPage
  render output

getPStart :: Handler_ RepHtml
getPStart = do
  (state, output) <- liftIO $ runWriterT mkGameState
  setState state
  render output
  -- redirect RedirectTemporary PRoot

getPGoto :: Link -> Handler_ ()
getPGoto link = do
  stepEngine (goto link)
  redirectWith temporaryRedirect307 PRoot

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

