{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Cyoa.Monad where

import Cyoa.PageLang
  
import Prelude hiding (take, drop)  

import Control.Monad.Writer  
import Control.Monad.RWS  
import Control.Monad.Error
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Array
import Data.Maybe
import Control.Applicative
import System.Random
  
data Link = PageLink PageNum
          | StartFightLink [Enemy] [PageItem] (Map Die Int)
          | ContinueFightLink (Maybe FightRound)
                        
data Output = OutputClear String [OutputItem]
            | OutputContinue [OutputItem]

instance Monoid Output where
  mempty = OutputContinue []
  (OutputContinue []) `mappend` output@(OutputClear _ _) = output
  (OutputClear title is) `mappend` (OutputContinue is') = OutputClear title (is `mappend` is')
  (OutputContinue is) `mappend` (OutputContinue is') = OutputContinue (is `mappend` is')
  
data OutputAttr = Good
                | Bad
                deriving Show
data OutputItem = OutText (Maybe OutputAttr) String
                | OutDie Int
                | OutLink Link String
                | OutBreak

outText = OutText Nothing                  
                  
data FightState = FS { fight_enemies :: [Enemy]
                     , fight_last_round :: Maybe FightRound
                     , fight_cont :: ([PageItem], Map Die Int) }
                deriving (Show)
  
data PlayerState = PS { player_carries :: Set Item
                      , player_flags :: Set Flag
                      , player_counters :: Map Counter Int                                           
                      , player_stats :: Map Stat (Int, Int) -- (current, initial)
                      , player_page :: PageNum
                      }
                 deriving (Show)

data GameState = GS { player_state :: PlayerState
                    , fight_state :: Maybe FightState
                    }
               deriving (Show)
                                                
newtype CyoaT m a = CyoaT { unCyoaT :: ErrorT String (RWST (Array PageNum Page) Output GameState m) a }
  deriving (Monad, Functor, MonadIO,
            MonadError String,
            MonadState GameState, MonadReader (Array PageNum Page), MonadWriter Output,
            Applicative)

-- RWS(T) muveletei:
-- Reader:
--   ask :: (Monad m) => CyoaT m (Array PageNum Page)
--   asks :: (Monad m) => (Array PageNum Page -> a) -> CyoaT m a
--   asks f = do
--     r <- ask
--     return $ f r
--
-- State:
--   get :: (Monad m) => CyoaT m PlayerState
--   gets :: (Monad m) => (PlayerState -> a) -> CyoaT m a
--   put :: (Monad m) => PlayerState -> CyoaT m ()
--   modify :: (Monad m) => (PlayerState -> PlayerState) -> CyoaT m ()

modifyPlayerState :: (Monad m) => (PlayerState -> PlayerState) -> CyoaT m ()
modifyPlayerState f = modify $ \gs -> gs{ player_state = f (player_state gs) }

modifyFightState :: (Monad m) => (FightState -> FightState) -> CyoaT m ()
modifyFightState f = modify $ \gs -> gs{ fight_state = f' (fight_state gs) }
  where f' = Just . f. fromJust
     
carries :: (Monad m) => Item -> CyoaT m Bool
carries item =
  gets $ Set.member item . player_carries . player_state

takeItem :: (Monad m) => Item -> CyoaT m ()
takeItem item =
  modifyPlayerState $ \ps -> ps { player_carries = Set.insert item (player_carries ps) }
        
dropItem :: (Monad m) => Item -> CyoaT m ()
dropItem item =
  modifyPlayerState $ \ps -> ps { player_carries = Set.delete item (player_carries ps) }

flagSet :: (Monad m) => Flag -> CyoaT m Bool
flagSet flag = gets $ Set.member flag . player_flags . player_state

setFlag :: (Monad m) => Flag -> CyoaT m ()
setFlag flag =        
  modifyPlayerState $ \ps -> ps { player_flags = Set.insert flag (player_flags ps) }

resetFlag :: (Monad m) => Flag -> CyoaT m ()
resetFlag flag =        
  modifyPlayerState $ \ps -> ps { player_flags = Set.delete flag (player_flags ps) }
           
getCounter :: (Monad m) => Counter -> CyoaT m Int
getCounter counter = do
  lookup <- gets $ Map.lookup counter . player_counters . player_state
  return $ 0 `fromMaybe` lookup
              
modifyCounter :: (Monad m) => (Int -> Int) -> Counter -> CyoaT m ()
modifyCounter f counter = do
  modifyPlayerState $ \ps -> ps { player_counters = Map.alter f' counter (player_counters ps) }
  where f' mx = Just $ f $ 0 `fromMaybe` mx
        
getStat :: (Monad m) => Stat -> CyoaT m Int
getStat a = gets $ fst . fromJust . Map.lookup a . player_stats . player_state

die :: (Monad m) => CyoaT m ()
die = do
  tell $ OutputContinue [outText "Kalandod itt véget ér."]
  throwError "dead"
            
modifyStat :: (Monad m) => (Int -> Int) -> Stat -> CyoaT m ()
modifyStat f stat = do
  modifyPlayerState $ \ps -> ps { player_stats = Map.alter (Just . f' . fromJust) stat (player_stats ps) }
  when (stat == Health) $ do
    health <- getStat Health
    when (health == 0) die  
                             
  where f' (current, initial) = (new, initial)
          where new = clamp (0, initial) (f current)
        clamp (low, high) x | x < low = low
                            | x > high = high
                            | otherwise = x
                      
data Attacker = AttackerPlayer
              | AttackerEnemy
              deriving Show

data FightRound = FightRound Attacker Bool
                deriving Show

roll :: (MonadIO m) => m Int
roll = liftIO $ randomRIO (1, 6)        
        
stepCyoa :: (Monad m) => CyoaT m a -> [Page] -> GameState -> m (Either String a, GameState, Output)
stepCyoa f pages gs = runRWST (runErrorT $ unCyoaT f) pageArray gs
  where pageArray = listArray (1, 400) pages
  
mkGameState :: IO GameState
mkGameState = do
  ps <- mkPlayer
  return $ GS { player_state = ps,
                fight_state = Nothing }
         
mkPlayer :: IO PlayerState
mkPlayer = do
  -- agility <- (6+) <$> roll
  -- health <- (12+) <$> ((+) <$> roll <*> roll)
  agility <- return 1
  health <- return 1
  luck <- (6+) <$> roll            
  return PS { player_carries = Set.empty,
              player_flags = Set.empty,
              player_counters = Map.empty,
                 
              player_stats = Map.fromList [ (Luck, (luck, luck))
                                          , (Agility, (agility, agility))
                                          , (Health, (health, health))],
              player_page = 73 } -- harc teszt: 73
