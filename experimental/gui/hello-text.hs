module Main where

import Graphics.UI.Gtk hiding (eventClick)
import Graphics.UI.Gtk.Gdk.Events
import Control.Monad.Trans
import Control.Monad.Reader

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

insertLinkToBuf buf text pageNum = do
  (start, end) <- insertTextToBuf buf text
  tag <- textTagNew Nothing
  set tag [ textTagForeground := "blue", textTagUnderline := UnderlineSingle ]
  onTextTagEvent tag $ \e iter -> do
    case e of
      Button{} -> when (eventClick e == ReleaseClick) $ print pageNum
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
  initGUI
  
  wnd <- windowNew
  on wnd deleteEvent $ do
    lift $ mainQuit
    return True
    
  buf <- textBufferNew Nothing  
  insertHeader buf "1."
  textBufferInsertAtCursor buf "Hello "
  insertLinkToBuf buf "World!" 42
  textBufferInsertAtCursor buf "\n"
  textBufferInsertAtCursor buf "This works!"
  
  
  tview <- textViewNewWithBuffer buf
  textViewSetEditable tview False
  textViewSetCursorVisible tview False
  
  widgetSetSizeRequest tview 400 600  
  
  containerAdd wnd tview
  
  widgetShowAll wnd  
  mainGUI
