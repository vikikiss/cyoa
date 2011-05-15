{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, OverloadedStrings #-}
import Yesod
import Control.Applicative ((<$>), (<*>))
import Data.Maybe
import Data.Text
import Control.Monad

import qualified Text.Blaze.Html4.Strict as HTML  
  
import Data.Set (Set)
import qualified Data.Set as Set
  
data GameState = GameState { items :: Set String,
                             page :: Int }
               deriving (Show, Read)
                        
data StateDemo = StateDemo
               
type Handler = GHandler StateDemo StateDemo
  
mkYesod "StateDemo" [$parseRoutes|                     
/ Root GET
/start Start GET
/goto/#Int Goto GET       
|]  

getState :: Handler GameState
getState = do
  state <- lookupSession "state"
  case state of
    Nothing -> redirect RedirectTemporary Start
    Just state -> return $ unserialize state

setState :: GameState -> Handler ()
setState = setSession "state" . serialize 
  
                  
getRoot :: Handler RepHtml
getRoot = do
  state <- getState
  case (page state) of
    1 -> do
      defaultLayout $ do
        when ("kard" `Set.member` (items state)) $
          addHtml $ HTML.p $ HTML.toHtml ("Van egy fasza kardod" :: String)
        addHamlet [$hamlet|
          <h1>#{page state}              
          <p><a href=@{Goto 2}>Menj a masodik oldalra
          |]
    2 -> do
      setState state{ items = Set.insert "kard" $ items state }
      defaultLayout [$hamlet|
                     <h1>#{page state}              
                     <p><a href=@{Goto 1}>Menj az elso oldalra
                     |]

getStart :: Handler ()
getStart = do
  setSession "state" $ serialize $ GameState { items = Set.empty, page = 1 }
  redirect RedirectTemporary Root

getGoto :: Int -> Handler ()  
getGoto n = do
  state <- getState
  setSession "state" $ serialize $ state{ page = n }
  redirect RedirectTemporary Root

serialize :: GameState -> Text
serialize = pack . show

unserialize :: Text -> GameState
unserialize = read . unpack
               
instance Yesod StateDemo where
    approot _ = ""
    -- clientSessionDuration _ = 60
main = warpDebug 3000 StateDemo
