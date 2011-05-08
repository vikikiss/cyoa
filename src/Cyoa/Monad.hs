{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Cyoa.Monad where

import Cyoa.PageLang
  
import Prelude hiding (take, drop)  

import Control.Monad.Writer  
import Control.Monad.RWS  
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Array
import Data.Maybe
import Control.Applicative
import System.Random
  
data FightState = FS { fight_enemies :: [Enemy]
                     , fight_last_round :: Maybe FightRound
                     , fight_cont :: ([PageItem], Map Die Int) }
                deriving (Show)
  
data PlayerState = PS { player_carries :: Set Item
                      , player_flags :: Set Flag
                      , player_counters :: Map Counter Int                                           
                      , player_stats :: Map Ability Int
                      , player_page :: PageNum
                      }
                 deriving (Show)

data GameState = GS { player_state :: PlayerState
                    , fight_state :: Maybe FightState
                    }
               deriving (Show)
                          
newtype CyoaT m a = CyoaT { unCyoaT :: RWST (Array PageNum Page) () GameState m a }
  deriving (Monad, Functor, MonadTrans, MonadIO, 
            MonadState GameState, MonadReader (Array PageNum Page),
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
        
getAbility :: (Monad m) => Ability -> CyoaT m Int
getAbility a = gets $ fromJust . Map.lookup a . player_stats . player_state
              
modifyAbility :: (Monad m) => (Int -> Int) -> Ability -> CyoaT m ()
modifyAbility f a = modifyPlayerState $ \ps -> ps { player_stats = Map.alter f' a (player_stats ps) }
  where f' = Just . f . fromJust

data Attacker = AttackerPlayer
              | AttackerEnemy
              deriving Show

data FightRound = FightRound Attacker Bool
                deriving Show

roll :: (MonadIO m) => m Int
roll = liftIO $ randomRIO (1, 6)        
        
stepCyoa :: CyoaT IO a -> [Page] -> GameState -> IO (a, GameState)
stepCyoa f pages gs = do
  (result, gs', output) <- runRWST (unCyoaT f) (listArray (1, 400) pages) gs
  return (result, gs')

mkGameState :: IO GameState
mkGameState = do
  ps <- mkPlayer
  return $ GS { player_state = ps,
                fight_state = Nothing }
         
mkPlayer :: IO PlayerState
mkPlayer = do
  agility <- (6+) <$> roll
  health <- (12+) <$> ((+) <$> roll <*> roll)
  luck <- (6+) <$> roll            
  return PS { player_carries = Set.empty,
              player_flags = Set.empty,
              player_counters = Map.empty,
                 
              player_stats = Map.fromList [(Luck, luck), (Agility, agility), (Health, health)],
              player_page = 73 } -- harc teszt: 73
