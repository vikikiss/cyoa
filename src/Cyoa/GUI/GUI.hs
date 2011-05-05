module Main where

import Graphics.UI.Gtk hiding (eventClick)
import Graphics.UI.Gtk.Gdk.Events
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.RWS

import Cyoa.Parser
import Cyoa.Engine
import Cyoa.PageLang
import Cyoa.Monad  
  
import System.Environment
import System.Exit
import System.IO

import Data.IORef
import System.IO.Unsafe  
  
-- TODO: always insert text to end of buffer  
insertTextToBuf buf text = do
  before <- textMarkNew Nothing True
  do            
    iterEnd <- textBufferGetEndIter buf
    textBufferAddMark buf before iterEnd  
    textBufferInsert buf iterEnd text
  start <- textBufferGetIterAtMark buf before
  end <- textBufferGetEndIter buf
  textBufferDeleteMark buf before
  return (start, end)

insertHeader buf text = do
  (start, end) <- insertTextToBuf buf text
  tag <- textTagNew Nothing
  set tag [ textTagScale := 1.6, textTagJustification := JustifyCenter ]
  textTagTableAdd `flip` tag =<< textBufferGetTagTable buf
  textBufferApplyTag buf tag start end
  textBufferInsertAtCursor buf "\n"

refState :: IORef GameState
refState = unsafePerformIO $ newIORef (error "refState")

refPages :: IORef [Page]
refPages = unsafePerformIO $ newIORef (error "refPages")

stepEngine f = do
  s <- readIORef refState
  pages <- readIORef refPages
  (x, s') <- stepCyoa f pages s
  writeIORef refState s'
  return x
           
insertLinkToBuf buf view text link = do
  (start, end) <- insertTextToBuf buf text
  tag <- textTagNew Nothing
  set tag [ textTagForeground := "blue", textTagUnderline := UnderlineSingle ]
  onTextTagEvent tag $ \e iter -> do
    case e of
      Button{} -> when (eventClick e == ReleaseClick) $ do
                    output <- stepEngine (goto link >> evalPage)
                    render buf view output
      Motion{} -> return () -- TODO: set mouse cursor on a DrawWindow
      _ -> return ()
  -- TODO: report bug: EAny-bol hogy lehet barmit kiolvasni?
  -- on tag textTagEvent $ \sender iter -> do
  --   e <- ask
  --   lift $ print e
  --   lift $ putStrLn "Foo"
  --   return True
  textTagTableAdd `flip` tag =<< textBufferGetTagTable buf
  textBufferApplyTag buf tag start end
  
main = do
  args <- initGUI
  pages <- case args of
    [filename] -> parsePages filename
    otherwise  -> do
      hPutStrLn stderr "Usage: helloworld <file.xml>"
      exitWith $ ExitFailure 1
  writeIORef refPages pages

  s0 <- mkGameState
  (output, s) <- stepCyoa `flip` pages `flip` s0 $ do
                   evalPage
  writeIORef refState s

  wnd <- windowNew
  on wnd deleteEvent $ do
    lift $ mainQuit
    return True
    
  buf <- textBufferNew Nothing    
  tview <- textViewNewWithBuffer buf
  render buf tview output
         
  set tview [textViewWrapMode := WrapWord, textViewPixelsAboveLines := 10,
             textViewLeftMargin := 10, textViewRightMargin := 10,
             textViewIndent := 10]
  textViewSetEditable tview False
  textViewSetCursorVisible tview False
  
  widgetSetSizeRequest tview 400 600  
  
  containerAdd wnd tview
  
  widgetShowAll wnd  
  mainGUI

render buf view (OutputClear title outItems) = do
  textBufferSetText buf ""
  insertHeader buf title
  render buf view (OutputContinue outItems)
  
render buf view (OutputContinue outItems) = do
  display outItems
    where
      display (OutBreak:os) = insertTextToBuf buf "\n" >> display os
      display ((OutText s):os) = insertTextToBuf buf s >> display os
      display ((OutLink link s):os) = insertLinkToBuf buf view s link >> display os
      display ((OutDie n):os) = do
        anchor <- textBufferCreateChildAnchor buf =<< textBufferGetEndIter buf
        roll <- buttonNewWithLabel "Dobj"
        on roll buttonActivated $ do
          set roll [widgetSensitive := False, buttonLabel := show n]              
          display os
        widgetShow roll
        textViewAddChildAtAnchor view roll anchor
      -- display ((OutFight enemies fight):os) = do
      --   textBufferInsertAtCursor buf $ show enemies
      --   let agent = FightAgent (const $ return False) (textBufferInsertAtCursor buf . show)
      --   stepEngine (fight agent)
      --   display os
      display [] = return ()
