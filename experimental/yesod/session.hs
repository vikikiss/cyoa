{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, OverloadedStrings #-}
import Yesod
import Control.Applicative ((<$>), (<*>))
import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as Map
  
data GameState = GameState { items :: Map String,
                             page :: Int }
               deriving (Show, Read)
                        
data Session = Session
type Handler = GHandler Session Session
mkYesod "Session" [$parseRoutes|
/ Root GET POST
/foo Foo GET  
|]
getRoot :: Handler RepHtml
getRoot = do
    sess <- getSession
    name <- lookupSession "name"
    hamletToRepHtml [$hamlet|
<h1>Hello #{fromMaybe "Dezsoke" name}!
<p>Ird be a nevedet:          
<form method=post
    <input type=text name=newName
    <input type=submit
<h1>#{show sess}
<h1><a href=@{Foo}>Menj mashova is!
|]

-- getFoo :: Handler RepHtml
getFoo = do
  name <- fromMaybe "Dezsoke" <$> lookupSession "name"
  defaultLayout [$hamlet|<h1>Szia #{name}|]
  

postRoot :: Handler ()
postRoot = do
    newName <- runFormPost' $ maybeStringInput "newName"
    case newName of
      Nothing -> return ()
      Just val -> setSession "name" val
    redirect RedirectTemporary Root

instance Yesod Session where
    approot _ = ""
    clientSessionDuration _ = 60
main = warpDebug 3000 Session
