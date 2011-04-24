module Main where

import Graphics.UI.Gtk hiding (eventClick)
import Graphics.UI.Gtk.Gdk.Events
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.RWS

import Cyoa.Parser
import Cyoa.Engine

import System.Environment
import System.Exit
import System.IO

import Data.IORef
import System.IO.Unsafe

insertTextToBuf buf text = do
  before <- textMarkNew Nothing True
  textBufferAddMark buf before =<< textBufferGetIterAtMark buf =<< textBufferGetInsert buf
  textBufferInsertAtCursor buf text
  after <- textBufferGetInsert buf  
    
  start <- textBufferGetIterAtMark buf before
  end <- textBufferGetIterAtMark buf after
  textBufferDeleteMark buf before
  return (start, end)

insertHeader buf text = do
  (start, end) <- insertTextToBuf buf text
  tag <- textTagNew Nothing
  set tag [ textTagScale := 1.6, textTagJustification := JustifyCenter ]
  textTagTableAdd `flip` tag =<< textBufferGetTagTable buf
  textBufferApplyTag buf tag start end
  textBufferInsertAtCursor buf "\n"

refState :: IORef PlayerState
refState = unsafePerformIO $ newIORef (error "refState")

refPages :: IORef [Page]
refPages = unsafePerformIO $ newIORef (error "refPages")

insertLinkToBuf buf view text pageNum = do
  (start, end) <- insertTextToBuf buf text
  tag <- textTagNew Nothing
  set tag [ textTagForeground := "blue", textTagUnderline := UnderlineSingle ]
  onTextTagEvent tag $ \e iter -> do
    case e of
      Button{} -> when (eventClick e == ReleaseClick) $ do
                    s <- readIORef refState
                    pages <- readIORef refPages
                    (output, s') <- stepCyoa (goto pageNum >> evalPage) pages s
                    writeIORef refState s'
                    render buf view pageNum output
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

  s0 <- mkPlayer             
  (output, s) <- stepCyoa `flip` pages `flip` s0 $ do
                   evalPage
  writeIORef refState s

  wnd <- windowNew
  on wnd deleteEvent $ do
    lift $ mainQuit
    return True
    
  buf <- textBufferNew Nothing    
  tview <- textViewNewWithBuffer buf
  render buf tview 1 output
         
  set tview [textViewWrapMode := WrapWord, textViewPixelsAboveLines := 10,
             textViewLeftMargin := 10, textViewRightMargin := 10,
             textViewIndent := 10]
  textViewSetEditable tview False
  textViewSetCursorVisible tview False
  
  widgetSetSizeRequest tview 400 600  
  
  containerAdd wnd tview
  
  widgetShowAll wnd  
  mainGUI

render buf view pageNum output = do
  textBufferSetText buf ""
  insertHeader buf (show pageNum ++ ".")
  display output
    where
      display (OutBreak:os) = textBufferInsertAtCursor buf "\n" >> display os
      display ((OutText s):os) = textBufferInsertAtCursor buf s >> display os
      display ((OutLink pageNum s):os) = insertLinkToBuf buf view s pageNum >> display os
      display ((OutDie n):os) = do
        anchor <- textBufferCreateChildAnchor buf =<< textBufferGetIterAtMark buf =<< textBufferGetInsert buf
        roll <- buttonNewWithLabel "Dobj"
        on roll buttonActivated $ do
          set roll [widgetSensitive := False, buttonLabel := show n]              
          display os
        widgetShow roll
        textViewAddChildAtAnchor view roll anchor
      display [] = return ()
