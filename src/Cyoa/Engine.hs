{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Cyoa.Engine where 

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
  
type Item = String
type Flag = String
type Counter = String
type Die = String -- Dobokocka

data Ability = Health -- Eletero
             | Agility -- Ugyesseg
             | Luck -- Szerencse
             deriving (Eq, Ord, Show)

type PageNum = Int

data PlayerState = PS { player_carries :: Set Item,
                        player_flags :: Set Flag,
                        player_counters :: Map Counter Int,
                                           
                        player_stats :: Map Ability Int,
                                        
                        player_page :: PageNum }
                 deriving Show

newtype CyoaT m a = CyoaT { unCyoaT :: RWST (Array PageNum Page) () PlayerState m a }
  deriving (Monad, Functor, MonadTrans, MonadState PlayerState, Applicative)

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

data Expr = ELiteral Int
          | Expr :+: Expr
          | Expr :-: Expr
          | Expr :*: Expr
          | DieRef Die
          | CounterRef Counter
          | AbilityQuery Ability
          deriving Show

evalExpr :: (Monad m, Functor m) => Expr -> CyoaT m Int
evalExpr (ELiteral n) = return n
evalExpr (e :+: f) = (+) <$> evalExpr e <*> evalExpr f
evalExpr (e :-: f) = (-) <$> evalExpr e <*> evalExpr f
evalExpr (e :*: f) = (*) <$> evalExpr e <*> evalExpr f
evalExpr (CounterRef counter) = getCounter counter                     
evalExpr (AbilityQuery a) = getAbility a
                                
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
          deriving Show

evalCond :: (Monad m, Functor m) => Cond -> CyoaT m Bool
evalCond (CLiteral b) = return b
evalCond (c :||: d) = (||) <$> evalCond c <*> evalCond d
evalCond (c :&&: d) = (&&) <$> evalCond c <*> evalCond d
evalCond (e :<: f) = (<) <$> evalExpr e <*> evalExpr f                      
evalCond (e :<=: f) = (<=) <$> evalExpr e <*> evalExpr f                      
evalCond (e :>: f) = (>) <$> evalExpr e <*> evalExpr f                      
evalCond (e :>=: f) = (>=) <$> evalExpr e <*> evalExpr f                      
evalCond (e :==: f) = (==) <$> evalExpr e <*> evalExpr f                      
evalCond (CNot c) = not <$> evalCond c
evalCond (Carry item) = carries item                    
                      
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
              | Set Flag
              | Reset Flag
              deriving Show
                       
type Output = [OutputItem]
data OutputItem = OutText String
                | OutLink PageNum String
                | OutBreak
                deriving Show
              
evalPage :: (Monad m, Functor m) => Page -> WriterT Output (CyoaT m) ()
evalPage (Page _ is) = mapM_ evalPageItem is

evalPageItem :: (Functor m, Monad m) => PageItem -> WriterT Output (CyoaT m) ()
evalPageItem (Paragraph is) = do
  mapM_ evalPageItem is
  tell [OutBreak]       
evalPageItem (TextLit s) = tell [OutText s]
evalPageItem (If c thn els) = do
  b <- lift $ evalCond c
  mapM_ evalPageItem (if b then thn else els)        
evalPageItem (Goto False pageNum) = do
  tell [OutLink pageNum $ unwords ["lapozz", if the then "a" else "az", show pageNum ++ ".", "oldalra"]]
  where the | pageNum `elem` [1, 5]  = False
            | pageNum `elem` [2, 3, 4, 6, 7, 8, 9] = True
            | pageNum < 50 = True
            | pageNum < 60 = False
            | otherwise = True                                                     
evalPageItem (Inc counter) = do
  lift $ modifyCounter succ counter
                          
-- goto :: PageNum -> CyoaM Page

test :: CyoaT IO ()                           
test = do
  let item = "sword"
      counter = "rings"
  (lift . print) =<< carries item
  (lift . print) =<< getCounter counter                   
  take item  
  (lift . print) =<< carries item                   
  drop item
  (lift . print) =<< carries item
  modifyCounter (+4) counter
  (lift . print) =<< getCounter counter
  (lift . print) =<< evalCond c
  modifyCounter (+1) counter
  take item                 
  (lift . print) =<< evalCond c
  where c = CounterRef "rings" :==: ELiteral 5 :&&: Carry "sword"
                

runCyoa :: CyoaT IO a -> IO a                   
runCyoa f = do
  (result, state', output) <- runRWST (unCyoaT f) undefined state
  return result
  where state = PS { player_carries = Set.empty,
                     player_flags = Set.empty,
                     player_counters = Map.empty,
                                       
                     player_stats = undefined,
                     player_page = 1 }
