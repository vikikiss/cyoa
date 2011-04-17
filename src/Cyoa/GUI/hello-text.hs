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
refState = unsafePerformIO $ newIORef undefined

refPages :: IORef [Page]
refPages = unsafePerformIO $ newIORef undefined

insertLinkToBuf buf text pageNum = do
  (start, end) <- insertTextToBuf buf text
  tag <- textTagNew Nothing
  set tag [ textTagForeground := "blue", textTagUnderline := UnderlineSingle ]
  onTextTagEvent tag $ \e iter -> do
    case e of
      Button{} -> when (eventClick e == ReleaseClick) $ do
                    s <- readIORef refState
                    pages <- readIORef refPages
                    (output, s') <- stepCyoa (goto pageNum >>= execWriterT . evalPage) pages s
                    writeIORef refState s'
                    render buf pageNum output
      Motion{} -> return () -- TODO: set mouse cursor
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

  (output, s0) <- runCyoa `flip` pages $ do
    page <- goto 1
    execWriterT $ evalPage page
  writeIORef refState s0

  wnd <- windowNew
  on wnd deleteEvent $ do
    lift $ mainQuit
    return True
    
  buf <- textBufferNew Nothing  
  render buf 1 output
  -- insertHeader buf "1."
  -- forM_ output $ \outputItem -> do
  --   case outputItem of
  --     OutBreak -> textBufferInsertAtCursor buf "\n"
  --     OutText s -> textBufferInsertAtCursor buf s
  --     OutLink pageNum s -> insertLinkToBuf buf s pageNum
  
  tview <- textViewNewWithBuffer buf
  set tview [textViewWrapMode := WrapWord, textViewPixelsAboveLines := 10,
             textViewLeftMargin := 10, textViewRightMargin := 10,
             textViewIndent := 10]
  textViewSetEditable tview False
  textViewSetCursorVisible tview False
  
  widgetSetSizeRequest tview 400 600  
  
  containerAdd wnd tview
  
  widgetShowAll wnd  
  mainGUI

render buf pageNum output = do
  textBufferSetText buf ""
  insertHeader buf (show pageNum ++ ".")
  forM_ output $ \outputItem -> do
    case outputItem of
      OutBreak -> textBufferInsertAtCursor buf "\n"
      OutText s -> textBufferInsertAtCursor buf s
      OutLink pageNum s -> insertLinkToBuf buf s pageNum
  
  
