{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, FlexibleContexts #-}
module Cyoa.Engine where

import Cyoa.PageLang
import Cyoa.Monad  
  
import Control.Monad.RWS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Array
import Data.Maybe
import Control.Applicative

newtype EvaluatorT m a = EvaluatorT { unEvaluatorT :: RWST [PageItem] () (Map Die Int) m a }
  deriving (Monad, MonadReader [PageItem], MonadState (Map Die Int), MonadIO, Applicative, Functor, MonadTrans)           

instance (MonadWriter w m) => MonadWriter w (EvaluatorT m) where
  tell = lift . tell
  listen f = do
    r <- ask
    s <- get
    ((x, s', _), w) <- (lift . listen) (runRWST (unEvaluatorT f) r s)
    put s'
    return (x, w)

  pass f = do
    r <- ask
    s <- get
    (x, s') <- lift . pass $ do
                 ((x, xform), s', _) <- runRWST (unEvaluatorT f) r s
                 return ((x, s'), xform)
    put s'
    return x

emit :: (MonadWriter Output m) => [OutputItem] -> m ()
emit = tell . OutputContinue    
    
evalExpr :: (Monad m, Functor m) => Expr -> EvaluatorT (CyoaT m) Int
evalExpr (ELiteral n) = return n
evalExpr (e :+: f) = (+) <$> evalExpr e <*> evalExpr f
evalExpr (e :-: f) = (-) <$> evalExpr e <*> evalExpr f
evalExpr (e :*: f) = (*) <$> evalExpr e <*> evalExpr f
evalExpr (e :%: f) = mod <$> evalExpr e <*> evalExpr f
evalExpr (DieRef die) = gets (fromJust . Map.lookup die)
evalExpr (CounterRef counter) = lift $ getCounter counter                     
evalExpr (StatQuery a) = lift $ getStat a
evalExpr (ECond cond thn els) = do
  b <- evalCond cond
  evalExpr (if b then thn else els)
                            
evalCond :: (Monad m, Functor m) => Cond -> EvaluatorT (CyoaT m) Bool
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
                      
evalPage :: (Functor m, MonadIO m) => CyoaT m ()
evalPage = do
  fightState <- gets fight_state
  case fightState of
    Nothing -> do
      pageNum <- gets $ player_page . player_state
      (Page _ is) <- asks (!pageNum)
      tell $ OutputClear (show pageNum ++ ".") []
      execRWST (unEvaluatorT $ evalPageItems is) is Map.empty
      return ()
    Just fs -> do
      fight      

applyLastRound :: (Functor m, MonadIO m) => CyoaT m ()
applyLastRound = do
  last_round <- gets (fight_last_round . fromJust . fight_state)
  case last_round of
    Nothing -> return ()
    Just (FightRound AttackerPlayer luck) -> do
      damage <- if not luck then return 2
                  else ifLucky (return 4) (return 1)
      emit [ OutBreak
           , outText "Megsebzed ellenfeledet "
           , outText (show damage)
           , outText " pont értékben! "
           ]
      hurtEnemy damage
      emit [ OutBreak ]
    Just (FightRound AttackerEnemy luck) -> do
      damage <- if not luck then return 2
                  else ifLucky (return 1) (return 3)
      emit [ OutBreak
           , outText "Ellenfeled megsebez "
           , outText (show damage)
           , outText " pont értékben! "
           ]
      modifyStat (\hp -> hp - damage) Health
      
  where
    ifLucky thn els = do
      d1 <- roll
      d2 <- roll
      luck <- getStat Luck
      modifyStat (\x -> x - 1) Luck                          
      let lucky = d1 + d2 <= luck
      emit [ OutDie d1
           , OutDie d2
           , if lucky then OutText (Just Good) "Micsoda mázli! " else OutText (Just Bad) "Pech. "
           ]
      if lucky then thn else els

    hurtEnemy damage = do
      (e:es) <- gets $ fight_enemies . fromJust . fight_state
      let e' = e{ enemy_health = (enemy_health e) - damage }
      if enemy_health e' <= 0
        then do
          modifyFightState $ \fs -> fs{ fight_enemies = es }
          emit [ outText $ unwords ["A(z)", enemy_name e, "holtan dől össze."] ] -- TODO: A(z)
        else 
          modifyFightState $ \fs -> fs{ fight_enemies = (e':es) }

calculateAttack :: (Functor m, MonadIO m) => Enemy -> CyoaT m ()
calculateAttack enemy = do
  emit [outText "Dobj a szörny nevében! "]
  enemyAttack <- rollAttack (enemy_agility enemy)
  emit [outText " A szörny támadóereje: ", outText (show enemyAttack), OutBreak]
       
  emit [outText "Dobj a saját támadásodért! "]
  playerAttack <- rollAttack =<< (getStat Agility)
  emit [outText " A Te támadóerőd: ", outText (show playerAttack), OutBreak]
  case enemyAttack `compare` playerAttack of
    EQ -> 
      emit [ outText "Kivédtétek egymás csapását! "
           , OutLink (ContinueFightLink Nothing) "Folytasd a csatát!"]
    LT -> do
      emit [OutText (Just Good) "Megsebezted a teremtményt. "]
      emitTryLuck AttackerPlayer
    GT -> do
      emit [OutText (Just Bad) "Megsebez a teremtmény! "]
      emitTryLuck AttackerEnemy
        
  where emitTryLuck attacker = do
          emit [ OutBreak
               , outText "Próbára teszed a szerencsédet? "
               , OutLink (ContinueFightLink (Just (FightRound attacker False))) "Nem."
               , outText " "
               , OutLink (ContinueFightLink (Just (FightRound attacker True))) "Igen."
               ]
                     
fight :: (Functor m, MonadIO m) => CyoaT m ()
fight = do
  applyLastRound

  fs <- gets $ fromJust . fight_state
  let enemies = fight_enemies fs
  case enemies of
    [] -> do
      let (is, dice) = fight_cont fs
      modify $ \gs -> gs{ fight_state = Nothing }
      execRWST (unEvaluatorT $ evalPageItems is) is dice
      return ()
      
    (enemy:_) -> do
      -- Debug kimenet
      forM_ enemies $ \e -> do
        emit [outText (show e), OutBreak]
      calculateAttack enemy
           
rollAttack agility = do    
    d1 <- roll
    d2 <- roll
    emit [OutDie d1, OutDie d2]
    return $ agility + d1 + d2

evalPageItems :: (Functor m, MonadIO m) => [PageItem] -> EvaluatorT (CyoaT m) ()
evalPageItems [] = return ()
evalPageItems (i:is) = do
  suspend <- local (const is) $ evalPageItem i
  if suspend then return () else evalPageItems is
           
evalPageItem :: (Functor m, MonadIO m) => PageItem -> EvaluatorT (CyoaT m) Bool
evalPageItem (Paragraph is) = do
  mapM_ evalPageItem is
  emit [OutBreak]
  return False
evalPageItem (TextLit s) = emit [outText s] >> return False
evalPageItem (If c thn els) = do
  b <- evalCond c
  mapM_ evalPageItem (if b then thn else els)
  return False
evalPageItem (Goto capitalize pageNum) = do
  emit [OutLink (PageLink pageNum) $ unwords [if capitalize then "Lapozz" else "lapozz", if the then "a" else "az", show pageNum ++ ".", "oldalra"]]
  return False
  where the | pageNum `elem` [1, 5]  = False
            | pageNum `elem` [2, 3, 4, 6, 7, 8, 9] = True
            | pageNum < 50 = True
            | pageNum < 60 = False
            | otherwise = True                                                     
evalPageItem (Inc counter) = lift $ modifyCounter succ counter >> return False                             
evalPageItem (Dec counter) = lift $ modifyCounter pred counter >> return False
evalPageItem (Clear counter) = lift $ modifyCounter (const 0) counter >> return False
evalPageItem (Take item) = lift $ takeItem item >> return False
evalPageItem (Drop item) = lift $ dropItem item >> return False
evalPageItem (GotoLucky refYes refNo) = do
  d1 <- roll
  d2 <- roll
  luck <- lift $ getStat Luck
  let page' | d1 + d2 <= luck = refYes
            | otherwise = refNo
  lift $ modifyStat pred Luck
  emit [outText "Tedd próbára SZERENCSÉDET!", OutDie d1, OutDie d2]
  evalPageItem (Goto True page')
evalPageItem (Damage stat expr) = do
  value <- evalExpr expr
  lift $ modifyStat (\x -> x - value) stat
  return False
evalPageItem (Heal stat expr) = do
  value <- evalExpr expr
  lift $ modifyStat (+value) stat
  return False
evalPageItem (Set flag) = lift $ setFlag flag >> return False
evalPageItem (DieDef name) = do
  n <- roll
  emit [OutDie n]
  modify (Map.insert name n)
  return False
evalPageItem (Fight enemies) = do
  is <- ask
  dice <- get
  emit [OutLink (StartFightLink enemies is dice) "Harcolj!"]
  return True
                               
goto :: (MonadIO m) => Link -> CyoaT m ()
goto (PageLink pageNum) =
  modifyPlayerState $ \ps -> ps{ player_page = pageNum }  
goto (StartFightLink enemies is dice) =
  modify $ \gs -> gs{ fight_state = Just fs }
    where fs = FS { fight_enemies = enemies,
                    fight_last_round = Nothing,
                    fight_cont = (is, dice) }
goto (ContinueFightLink round) = 
  modifyFightState (\fs -> fs{ fight_last_round = round })

