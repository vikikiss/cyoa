{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Cyoa.Engine (
  Page(..), PageItem(..), Expr(..), Cond(..), Enemy(..),
  Output(..), OutputItem(..), 

  Ability(..),
  PlayerState, stepCyoa,

  -- FightAgent(..), Attacker(..), FightEvent(..),
             
  mkPlayer, evalPage, goto) where 

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
  
type Item = String
type Flag = String
type Counter = String
type Die = String -- Dobokocka

data Ability = Health -- Eletero
             | Agility -- Ugyesseg
             | Luck -- Szerencse
             deriving (Eq, Ord, Show, Read)

type PageNum = Int

data FightState = FS { fight_enemies :: [Enemy]
                     , fight_last_round :: Maybe FightRound}
                deriving (Show)
  
data PlayerState = PS { player_carries :: Set Item
                      , player_flags :: Set Flag
                      , player_counters :: Map Counter Int                                           
                      , player_stats :: Map Ability Int
                      , player_fight :: Maybe FightState
                      , player_page :: PageNum
                      }
                 deriving (Show)

newtype CyoaT m a = CyoaT { unCyoaT :: RWST (Array PageNum Page) () PlayerState m a }
  deriving (Monad, Functor, MonadTrans, MonadIO, 
            MonadState PlayerState, MonadReader (Array PageNum Page),
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
  
carries :: (Monad m) => Item -> CyoaT m Bool
carries item =
  gets $ Set.member item . player_carries

take :: (Monad m) => Item -> CyoaT m ()
take item =
  modify $ \playerState -> playerState { player_carries = Set.insert item (player_carries playerState) }
        
drop :: (Monad m) => Item -> CyoaT m ()
drop item =
  modify $ \playerState -> playerState { player_carries = Set.delete item (player_carries playerState) }

flagSet :: (Monad m) => Flag -> CyoaT m Bool
flagSet flag = gets $ Set.member flag . player_flags

set :: (Monad m) => Flag -> CyoaT m ()
set flag =        
  modify $ \playerState -> playerState { player_flags = Set.insert flag (player_flags playerState) }

reset :: (Monad m) => Flag -> CyoaT m ()
reset flag =        
  modify $ \playerState -> playerState { player_flags = Set.delete flag (player_flags playerState) }
           
getCounter :: (Monad m) => Counter -> CyoaT m Int
getCounter counter = do
  lookup <- gets $ Map.lookup counter . player_counters
  return $ 0 `fromMaybe` lookup
              
modifyCounter :: (Monad m) => (Int -> Int) -> Counter -> CyoaT m ()
modifyCounter f counter = do
  modify $ \playerState -> playerState { player_counters = Map.alter f' counter (player_counters playerState) }
  where f' mx = Just $ f $ 0 `fromMaybe` mx
        
getAbility :: (Monad m) => Ability -> CyoaT m Int
getAbility a = gets $ fromJust . Map.lookup a . player_stats
              
modifyAbility :: (Monad m) => (Int -> Int) -> Ability -> CyoaT m ()
modifyAbility f a = modify $ \playerState -> playerState { player_stats = Map.alter f' a (player_stats playerState) }
  where f' = Just . f . fromJust

type EvaluatorT m a = RWST () [OutputItem] (Map Die Int) (CyoaT m) a                         
                         
data Expr = ELiteral Int
          | Expr :+: Expr
          | Expr :-: Expr
          | Expr :*: Expr
          | Expr :%: Expr
          | DieRef Die
          | ECond Cond Expr Expr -- ?:
          | CounterRef Counter
          | AbilityQuery Ability
          deriving Show

evalExpr :: (Monad m, Functor m) => Expr -> EvaluatorT m Int
evalExpr (ELiteral n) = return n
evalExpr (e :+: f) = (+) <$> evalExpr e <*> evalExpr f
evalExpr (e :-: f) = (-) <$> evalExpr e <*> evalExpr f
evalExpr (e :*: f) = (*) <$> evalExpr e <*> evalExpr f
evalExpr (e :%: f) = mod <$> evalExpr e <*> evalExpr f
evalExpr (DieRef die) = gets (fromJust . Map.lookup die)
evalExpr (CounterRef counter) = lift $ getCounter counter                     
evalExpr (AbilityQuery a) = lift $ getAbility a
evalExpr (ECond cond thn els) = do
  b <- evalCond cond
  evalExpr (if b then thn else els)
                            
data Cond = CLiteral Bool
          | Cond :||: Cond
          | Cond :&&: Cond
          | CNot Cond
          | Expr :<: Expr
          | Expr :<=: Expr
          | Expr :>: Expr
          | Expr :>=: Expr
          | Expr :==: Expr
          | Carry Item
          | FlagSet Flag
          deriving Show

evalCond :: (Monad m, Functor m) => Cond -> EvaluatorT m Bool
evalCond (CLiteral b) = return b
evalCond (c :||: d) = (||) <$> evalCond c <*> evalCond d
evalCond (c :&&: d) = (&&) <$> evalCond c <*> evalCond d
evalCond (e :<: f) = (<) <$> evalExpr e <*> evalExpr f                      
evalCond (e :<=: f) = (<=) <$> evalExpr e <*> evalExpr f                      
evalCond (e :>: f) = (>) <$> evalExpr e <*> evalExpr f                      
evalCond (e :>=: f) = (>=) <$> evalExpr e <*> evalExpr f                      
evalCond (e :==: f) = (==) <$> evalExpr e <*> evalExpr f                      
evalCond (CNot c) = not <$> evalCond c
evalCond (Carry item) = lift $ carries item                    
evalCond (FlagSet flag) = lift $ flagSet flag
                      
data Page = Page PageNum [PageItem]
          deriving Show
                   
data PageItem = Paragraph [PageItem]
              | TextLit String
              | If Cond [PageItem] [PageItem]
              | Goto Bool PageNum
              | GotoLucky PageNum PageNum
              | DieDef Die
              | Heal Ability Expr
              | Damage Ability Expr
              | Take Item
              | Inc Counter
              | Dec Counter
              | Clear Counter
              | Set Flag
              | Reset Flag
              | Fight [Enemy]
              deriving Show

data Enemy = Enemy { enemy_name :: String,
                     enemy_agility :: Int,
                     enemy_health :: Int }
             deriving Show

-- data Attacker = AttackerEnemy String
--               | AttackerPlayer
--               deriving Show
-- data FightAgent m = FightAgent (Attacker -> m Bool) (FightEvent -> m ())
-- data FightEvent = Hit Attacker Int
--                 | Draw
--                 | PlayerDied
--                 | EnemyDied String
--                 | Rolled Int
--                 deriving Show

-- fight :: (Functor m, MonadIO m) => FightAgent IO -> [Enemy] -> CyoaT m ()
-- fight _ [] = return ()
-- fight agent@(FightAgent tryLuck notify) (e:es) = do
--   enemy_attack <- (+) (enemy_agility e) <$> ((+) <$> roll <*> roll)
--   player_attack <- (+) <$> getAbility Agility <*> ((+) <$> roll <*> roll)
--   case enemy_attack `compare` player_attack of
--     EQ -> do
--       liftIO $ notify Draw
--       fight agent (e:es)
--     LT -> do
--       shouldTryLuck <- liftIO $ tryLuck AttackerPlayer
--       damage <- 
--         if shouldTryLuck
--           then do
--             lucky <- hasLuck
--             return (if lucky then 4 else 1)
--           else return 2
--       liftIO $ notify $ Hit AttackerPlayer damage
--       let e' = e{ enemy_health = enemy_health e - damage }
--       if enemy_health e' <= 0
--         then do
--           liftIO $ notify $ EnemyDied (enemy_name e)
--           fight agent es
--         else fight agent (e':es)
--     GT -> do
--       shouldTryLuck <- liftIO $ tryLuck (AttackerEnemy (enemy_name e))
--       damage <- 
--         if shouldTryLuck
--           then do
--             lucky <- hasLuck
--             return (if lucky then 1 else 3)
--           else return 2
--       liftIO $ notify $ Hit (AttackerEnemy (enemy_name e)) damage
--       modifyAbility (\x -> x - damage) Health
--       health <- getAbility Health
--       if health <= 0
--          then liftIO $ notify $ PlayerDied
--          else fight agent (e:es)
--   where hasLuck = do
--           d1 <- roll
--           liftIO $ notify (Rolled d1)
--           d2 <- roll
--           liftIO $ notify (Rolled d2)
--           luck <- getAbility Luck
--           modifyAbility (\x -> x - 1) Luck                          
--           return $ d1 + d2 <= luck          

data Attacker = AttackerPlayer
              | AttackerEnemy
              deriving Show

data Link = PageLink PageNum
          | StartFightLink [Enemy]
          | ContinueFightLink (Maybe FightRound)
            
data FightRound = FightRound Attacker Bool
                deriving Show

data Output = Output String [OutputItem]
data OutputItem = OutText String
                | OutDie Int
                | OutLink Link String
                | OutBreak
                -- | OutFight [Enemy] (FightAgent IO -> CyoaT IO ())

evalPage :: (Functor m, MonadIO m) => CyoaT m Output
evalPage = do
  fightState <- gets player_fight
  case fightState of
    Nothing -> do
      pageNum <- gets player_page
      (Page _ is) <- asks (!pageNum)
      (_, w) <- execRWST `flip` () `flip` Map.empty $ do              
                     mapM_ evalPageItem is
      return $ Output (show pageNum ++ ".") w                    
    Just fs -> do
      w <- execWriterT fight
      return $ Output "Csata!" w

fight :: (Functor m, MonadIO m) => WriterT [OutputItem] (CyoaT m) ()
fight = do
  last_round <- gets (fight_last_round . fromJust . player_fight)
  case last_round of
    Nothing -> return ()
    Just (FightRound AttackerPlayer luck) -> do
      damage <- if not luck then return 2
                  else do
                    lucky <- hasLuck
                    return $ if lucky then 4 else 1
      tell [OutText "Megsebzed ellenfeledet "
           ,OutText (show damage)
           ,OutText " pont értékben!"]
      lift $ modifyFightState (hurtEnemy damage)
    Just (FightRound AttackerEnemy luck) -> do
      damage <- if not luck then return 2
                  else do
                    lucky <- hasLuck
                    return $ if lucky then 1 else 3
      tell [OutText "Ellenfeled megsebez "
           ,OutText (show damage)
           ,OutText " pont értékben!"]
      -- TODO: jatekos serulese
      
  enemies <- lift $ gets (fight_enemies . fromJust . player_fight)         
  let (enemy:_) = enemies
  forM_ enemies $ \e -> do
    tell [OutText (show e), OutBreak]
  tell [OutText "Dobj a szörny nevében!"]
  enemyAttack <- rollAttack (enemy_agility enemy)
  tell [OutText "A szörny támadóereje: ", OutText (show enemyAttack)]
       
  tell [OutText "Dobj a saját támadásodért!"]
  playerAttack <- rollAttack =<< (lift $ getAbility Agility)
  tell [OutText "A Te támadóerőd: ", OutText (show playerAttack)]
  case enemyAttack `compare` playerAttack of
    EQ -> do
      tell [ OutText "Kivédtétek egymás csapását!"
           , OutLink (ContinueFightLink Nothing) "Folytasd a csatát!"]
    LT -> do
      tell [ OutText "Megsebezted a teremtményt."
           , OutText "Próbára teszed a szerencsédet?"
           , OutLink (ContinueFightLink (Just (FightRound AttackerPlayer False))) "Nem."
           , OutText " "
           , OutLink (ContinueFightLink (Just (FightRound AttackerPlayer True))) "Igen."
           ]
    GT -> do
      tell [ OutText "Megsebez a teremtmény!"
           , OutText "Próbára teszed a szerencsédet?"
           , OutLink (ContinueFightLink (Just (FightRound AttackerEnemy False))) "Nem."
           , OutText " "
           , OutLink (ContinueFightLink (Just (FightRound AttackerEnemy True))) "Igen."
           ]
      
  where hasLuck = do
          d1 <- roll
          d2 <- roll
          luck <- lift $ getAbility Luck
          lift $ modifyAbility (\x -> x - 1) Luck                          
          let lucky = d1 + d2 <= luck
          tell [OutDie d1, OutDie d2, OutText $ if lucky then "Micsoda mázli!" else "Pech."]
          return lucky
        hurtEnemy damage fs@FS{ fight_enemies = (e:es) } = fs{ fight_enemies = (e':es) }
          where e' = e{ enemy_health = (enemy_health e) - damage }
           
rollAttack agility = do    
    d1 <- roll
    d2 <- roll
    tell [OutDie d1, OutDie d2]
    return $ agility + d1 + d2
    
evalPageItem :: (Functor m, MonadIO m) => PageItem -> EvaluatorT m ()
evalPageItem (Paragraph is) = do
  mapM_ evalPageItem is
  tell [OutBreak]       
evalPageItem (TextLit s) = tell [OutText s]
evalPageItem (If c thn els) = do
  b <- evalCond c
  mapM_ evalPageItem (if b then thn else els)        
evalPageItem (Goto capitalize pageNum) = do
  tell [OutLink (PageLink pageNum) $ unwords [if capitalize then "Lapozz" else "lapozz", if the then "a" else "az", show pageNum ++ ".", "oldalra"]]
  where the | pageNum `elem` [1, 5]  = False
            | pageNum `elem` [2, 3, 4, 6, 7, 8, 9] = True
            | pageNum < 50 = True
            | pageNum < 60 = False
            | otherwise = True                                                     
evalPageItem (Inc counter) = lift $ modifyCounter succ counter
evalPageItem (Dec counter) = lift $ modifyCounter pred counter
evalPageItem (Clear counter) = lift $ modifyCounter (const 0) counter
evalPageItem (Take item) = lift $ take item
evalPageItem (GotoLucky refYes refNo) = do
  d1 <- roll
  d2 <- roll
  luck <- lift $ getAbility Luck
  let page' | d1 + d2 <= luck = refYes
            | otherwise = refNo
  lift $ modifyAbility (\x -> x - 1) Luck
  tell [OutText "Tedd próbára SZERENCSÉDET!", OutDie d1, OutDie d2]
  evalPageItem (Goto True page')
evalPageItem (Damage ability expr) = do
  value <- evalExpr expr
  lift $ modifyAbility (\x -> x - value) ability
evalPageItem (Heal ability expr) = do
  value <- evalExpr expr
  lift $ modifyAbility (+value) ability
evalPageItem (Set flag) = lift $ set flag
evalPageItem (DieDef name) = do
  n <- roll
  tell [OutDie n]
  modify (Map.insert name n)
evalPageItem (Fight enemies) = do
  tell [OutLink (StartFightLink enemies) "Harcolj!"]  
-- evalPageItem (Fight enemies) = do
--   tell [OutFight enemies $ fight `flip` enemies]
                               
roll :: (MonadIO m) => m Int
roll = liftIO $ randomRIO (1, 6)        
        
goto :: (MonadIO m) => Link -> CyoaT m ()
goto (PageLink pageNum) =
  modify $ \ playerState -> playerState{ player_page = pageNum }  
goto (StartFightLink enemies) =
  modify $ \ playerState -> playerState{ player_fight = Just fs }
    where fs = FS { fight_enemies = enemies,
                    fight_last_round = Nothing }
goto (ContinueFightLink round) = 
  modifyFightState (\fs -> fs{ fight_last_round = round })

modifyFightState :: (Monad m) => (FightState -> FightState) -> CyoaT m ()
modifyFightState f = do
  modify (\ps -> ps{ player_fight = Just . f . fromJust $ player_fight ps })
                              
stepCyoa :: CyoaT IO a -> [Page] -> PlayerState -> IO (a, PlayerState)
stepCyoa f pages s = do
  (result, s', output) <- runRWST (unCyoaT f) (listArray (1, 400) pages) s
  return (result, s')

mkPlayer :: IO PlayerState
mkPlayer = do
  agility <- (6+) <$> roll
  health <- (12+) <$> ((+) <$> roll <*> roll)
  luck <- (6+) <$> roll            
  return PS { player_carries = Set.empty,
              player_flags = Set.empty,
              player_counters = Map.empty,
                 
              player_stats = Map.fromList [(Luck, luck), (Agility, agility), (Health, health)],
              player_fight = Nothing,
              player_page = 73 } -- harc teszt: 73
