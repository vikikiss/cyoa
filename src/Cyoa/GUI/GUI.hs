module Main where

import Data.Map (Map)
import qualified Data.Map as Map

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

import Paths_Cyoa
import System.Environment
import System.Exit
import System.IO

import Data.IORef
import System.IO.Unsafe

data GUICtxt = GUICtxt { gui_buf :: TextBuffer
                       , gui_view :: TextView
                       , gui_stat_labels :: Map Stat Label
                       }

type GUI a = ReaderT GUICtxt IO a

-- TODO: always insert text to end of buffer
insertTextToBuf :: String -> GUI (TextIter, TextIter)
insertTextToBuf text = do
  buf <- asks gui_buf
  view <- asks gui_view
  lift $ do
    before <- textMarkNew Nothing True
    do
      iterEnd <- textBufferGetEndIter buf
      textBufferAddMark buf before iterEnd
      textBufferInsert buf iterEnd text
    start <- textBufferGetIterAtMark buf before
    end <- textBufferGetEndIter buf
    textBufferDeleteMark buf before
    Just scrollMark <- textBufferGetMark buf "scroll"
    textViewScrollToMark view scrollMark 0 (Just (0, 0))
    return (start, end)

insertHeader text = do
  (start, end) <- insertTextToBuf text
  buf <- asks gui_buf
  lift $ do
    textBufferApplyTagByName buf "header" start end
    textBufferInsertAtCursor buf "\n"

refState :: IORef GameState
refState = unsafePerformIO $ newIORef (error "refState")

refPages :: IORef [Page]
refPages = unsafePerformIO $ newIORef (error "refPages")

refLinkCount :: IORef Int
refLinkCount = unsafePerformIO $ newIORef (error "refLinkCount")

stepEngine :: CyoaT IO a -> GUI (Either GameEvent a, Output)
stepEngine f = do
  s <- lift $ readIORef refState
  pages <- lift $ readIORef refPages
  (x, s', w) <- lift $ stepCyoa f pages s
  lift $ writeIORef refState s'
  return (x, w)

insertLinkToBuf text link = do
  buf <- asks gui_buf
  ctx <- ask
  linkCount <- lift $ readIORef refLinkCount
  (start, end) <- insertTextToBuf text
  lift $ do
    tag <- textTagNew Nothing
    set tag [ textTagForeground := "blue", textTagUnderline := UnderlineSingle ]
    textTagTableAdd `flip` tag =<< textBufferGetTagTable buf
    textBufferApplyTag buf tag start end

    onTextTagEvent tag $ \e iter -> do
      case e of
        Button{} -> when (eventClick e == ReleaseClick) $ do
                      linkCount' <- readIORef refLinkCount
                      when (linkCount == linkCount') $ runReaderT `flip` ctx $ do
                        (result, output) <- stepEngine (goto link >> evalPage)
                        render output
                        case result of
                          Left DeathEvent -> liftIO $ putStrLn "Meghaltal"
                          Left WinEvent -> liftIO $ putStrLn "Nyertel"
                          _ -> return ()
        Motion{} -> return () -- TODO: set mouse cursor on a DrawWindow
        _ -> return ()
    -- TODO: report bug: EAny-bol hogy lehet barmit kiolvasni?
    -- on tag textTagEvent $ \sender iter -> do
    --   e <- ask
    --   lift $ print e
    --   lift $ putStrLn "Foo"
    --   return True

setupTextBuf buf = do
  addTag "header" [ textTagScale := 1.6, textTagJustification := JustifyCenter ]
  addTag "good" [ textTagForeground := "green" ]
  addTag "bad" [ textTagForeground := "red" ]
    where addTag name attrs = do
            tag <- textTagNew (Just name)
            set tag attrs
            textTagTableAdd `flip` tag =<< textBufferGetTagTable buf


main = do
  args <- initGUI
  pages <- case args of
    [filename] -> parsePages filename
    otherwise  -> do
      self <- getProgName
      hPutStrLn stderr $ unwords ["Usage:", self, "<file.xml>"]
      exitWith $ ExitFailure 1
  writeIORef refPages pages

  wnd <- windowNew
  on wnd deleteEvent $ do
    lift $ mainQuit
    return True

  buf <- textBufferNew Nothing
  setupTextBuf buf
  tview <- textViewNewWithBuffer buf

  scrollMark <- textMarkNew (Just "scroll") False
  end <- textBufferGetEndIter buf
  textBufferAddMark buf scrollMark end

  set tview [textViewWrapMode := WrapWord, textViewPixelsAboveLines := 10,
             textViewLeftMargin := 10, textViewRightMargin := 10,
             textViewIndent := 10]
  textViewSetEditable tview False
  textViewSetCursorVisible tview False

  widgetSetSizeRequest tview 400 600

  hbox <- hBoxNew False 0
  image <- imageNewFromFile =<< getDataFileName "Hero42.jpg"
  containerAdd hbox image


  scrollwin <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scrollwin PolicyNever PolicyAutomatic
  containerAdd scrollwin tview

  statusbar <- statusbarNew
  let statusLabel icon = do
         hbox <- hBoxNew False 0
         path <- getDataFileName icon
         containerAdd hbox =<< imageNewFromFile path
         label <- labelNew Nothing
         containerAdd hbox label
         boxPackStart statusbar hbox PackNatural 0
         return label

  labelHealth <- statusLabel "heart_icon.png"
  labelAgility <- statusLabel "sword_icon.png"
  labelLuck <- statusLabel "luck_icon.png"

  let stat_labels = Map.fromList [(Health, labelHealth), (Agility, labelAgility), (Luck, labelLuck)]

  vbox <- vBoxNew False 0
  containerAdd vbox scrollwin
  containerAdd vbox statusbar

  containerAdd hbox vbox
  containerAdd wnd hbox

  widgetShowAll wnd

  (s0, output0) <- runWriterT mkGameState
  -- TODO: stepEngine
  -- (_, s, output) <- stepCyoa `flip` pages `flip` s0 $ do
  --                     evalPage
  writeIORef refState s0
  runReaderT `flip` (GUICtxt buf tview stat_labels) $ do
    render output0
  mainGUI

render (OutputClear title outItems) = do
  buf <- asks gui_buf
  lift $ writeIORef refLinkCount 0
  lift $ textBufferSetText buf ""
  insertHeader title
  render (OutputContinue outItems)
render (OutputContinue outItems) = do
  lift $ modifyIORef refLinkCount succ

  (Right state, _) <- stepEngine (gets id)

  stat_labels <- asks gui_stat_labels
  forM_ (Map.toList stat_labels) $ \(stat, label) -> do
    (Right value, _) <- stepEngine (getStat stat)
    lift $ labelSetText label (show value)

  display outItems
    where
      display (OutBreak:os) = insertTextToBuf "\n" >> display os
      display ((OutEnemies enemies):os) = do
        forM_ enemies $ \(Enemy name agility health) -> do
          display [ OutText Nothing $ unwords [name, "\t"
                                              , "Ügyesség:", show agility
                                              , "Életerő:", show health
                                              ]
                  , OutBreak]
        display os
      display ((OutText a s):os) = do
        buf <- asks gui_buf
        (start, end) <- insertTextToBuf s
        case a of
          Nothing -> return ()
          Just attr -> lift $ textBufferApplyTagByName buf tagName start end
            where tagName = case attr of
                              Good -> "good"
                              Bad -> "bad"
        display os
      display ((OutLink link s):os) = insertLinkToBuf s link >> display os
      display ((OutDie n):os) = do
        buf <- asks gui_buf
        view <- asks gui_view
        ctx <- ask
        lift $ do
          anchor <- textBufferCreateChildAnchor buf =<< textBufferGetEndIter buf
          roll <- buttonNewWithLabel "Dobj"
          on roll buttonActivated $ do
            set roll [widgetSensitive := False, buttonLabel := show n]
            runReaderT (display os) ctx
          widgetShow roll
          textViewAddChildAtAnchor view roll anchor
      display [] = return ()
