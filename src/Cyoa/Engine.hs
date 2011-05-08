module Cyoa.Engine where

import Cyoa.PageLang
import Cyoa.Monad  
  
import Control.Monad.Writer  
import Control.Monad.RWS  
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Array
import Data.Maybe
import Control.Applicative
  
type EvaluatorT m a = RWST [PageItem] [OutputItem] (Map Die Int) (CyoaT m) a                         
                         
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
                      
data Link = PageLink PageNum
          | StartFightLink [Enemy] [PageItem] (Map Die Int)
          | ContinueFightLink (Maybe FightRound)
                        
data Output = OutputClear String [OutputItem]
            | OutputContinue [OutputItem]
data OutputAttr = Good
                | Bad
                deriving Show
data OutputItem = OutText (Maybe OutputAttr) String
                | OutDie Int
                | OutLink Link String
                | OutBreak

outText = OutText Nothing                  
                  
evalPage :: (Functor m, MonadIO m) => CyoaT m Output
evalPage = do
  fightState <- gets fight_state
  case fightState of
    Nothing -> do
      pageNum <- gets $ player_page . player_state
      (Page _ is) <- asks (!pageNum)
      (_, w) <- execRWST (evalPageItems is) is Map.empty
      return $ OutputClear (show pageNum ++ ".") w                    
    Just fs -> do
      w <- execWriterT fight
      return $ OutputContinue w

applyLastRound :: (Functor m, MonadIO m) => WriterT [OutputItem] (CyoaT m) ()
applyLastRound = do
  last_round <- gets (fight_last_round . fromJust . fight_state)
  case last_round of
    Nothing -> return ()
    Just (FightRound AttackerPlayer luck) -> do
      damage <- if not luck then return 2
                  else ifLucky (return 4) (return 1)
      tell [ OutBreak
           , outText "Megsebzed ellenfeledet "
           , outText (show damage)
           , outText " pont értékben! "]
      hurtEnemy damage
      tell [ OutBreak ]
    Just (FightRound AttackerEnemy luck) -> do
      damage <- if not luck then return 2
                  else ifLucky (return 1) (return 3)
      tell [ OutBreak
           , outText "Ellenfeled megsebez "
           , outText (show damage)
           , outText " pont értékben! "]
      -- TODO: jatekos serulese
      
  where
    ifLucky thn els = do
      d1 <- roll
      d2 <- roll
      luck <- lift $ getAbility Luck
      lift $ modifyAbility (\x -> x - 1) Luck                          
      let lucky = d1 + d2 <= luck
      tell [OutDie d1, OutDie d2, if lucky then OutText (Just Good) "Micsoda mázli! " else OutText (Just Bad) "Pech. "]
      if lucky then thn else els

    hurtEnemy damage = do
      (e:es) <- lift $ gets $ fight_enemies . fromJust . fight_state
      let e' = e{ enemy_health = (enemy_health e) - damage }
      if enemy_health e' <= 0
        then do
          lift $ modifyFightState $ \fs -> fs{ fight_enemies = es }
          tell [ outText $ unwords ["A(z)", enemy_name e, "holtan dől össze."] ] -- TODO: A(z)
        else 
          lift $ modifyFightState $ \fs -> fs{ fight_enemies = (e':es) }

calculateAttack :: (Functor m, MonadIO m) => Enemy -> WriterT [OutputItem] (CyoaT m) ()
calculateAttack enemy = do
  tell [outText "Dobj a szörny nevében! "]
  enemyAttack <- rollAttack (enemy_agility enemy)
  tell [outText " A szörny támadóereje: ", outText (show enemyAttack), OutBreak]
       
  tell [outText "Dobj a saját támadásodért! "]
  playerAttack <- rollAttack =<< (lift $ getAbility Agility)
  tell [outText " A Te támadóerőd: ", outText (show playerAttack), OutBreak]
  case enemyAttack `compare` playerAttack of
    EQ -> 
      tell [ outText "Kivédtétek egymás csapását! "
           , OutLink (ContinueFightLink Nothing) "Folytasd a csatát!"]
    LT -> do
      tell [OutText (Just Good) "Megsebezted a teremtményt. "]
      tellTryLuck AttackerPlayer
    GT -> do
      tell [OutText (Just Bad) "Megsebez a teremtmény! "]
      tellTryLuck AttackerEnemy
        
  where tellTryLuck attacker = do
          tell [ OutBreak
               , outText "Próbára teszed a szerencsédet? "
               , OutLink (ContinueFightLink (Just (FightRound attacker False))) "Nem."
               , outText " "
               , OutLink (ContinueFightLink (Just (FightRound attacker True))) "Igen."
               ]
                     
fight :: (Functor m, MonadIO m) => WriterT [OutputItem] (CyoaT m) ()
fight = do
  applyLastRound

  fs <- lift $ gets $ fromJust . fight_state
  let enemies = fight_enemies fs
  case enemies of
    [] -> do
      let (is, dice) = fight_cont fs
      modify $ \gs -> gs{ fight_state = Nothing }
      (_, w) <- lift $ execRWST (evalPageItems is) is dice
      tell w
      return ()
      
    (enemy:_) -> do
      -- Debug kimenet
      forM_ enemies $ \e -> do
        tell [outText (show e), OutBreak]
      calculateAttack enemy
           
rollAttack agility = do    
    d1 <- roll
    d2 <- roll
    tell [OutDie d1, OutDie d2]
    return $ agility + d1 + d2

evalPageItems :: (Functor m, MonadIO m) => [PageItem] -> EvaluatorT m ()
evalPageItems [] = return ()
evalPageItems (i:is) = do
  suspend <- local (const is) $ evalPageItem i
  if suspend then return () else evalPageItems is
           
evalPageItem :: (Functor m, MonadIO m) => PageItem -> EvaluatorT m Bool
evalPageItem (Paragraph is) = do
  mapM_ evalPageItem is
  tell [OutBreak]
  return False
evalPageItem (TextLit s) = tell [outText s] >> return False
evalPageItem (If c thn els) = do
  b <- evalCond c
  mapM_ evalPageItem (if b then thn else els)
  return False
evalPageItem (Goto capitalize pageNum) = do
  tell [OutLink (PageLink pageNum) $ unwords [if capitalize then "Lapozz" else "lapozz", if the then "a" else "az", show pageNum ++ ".", "oldalra"]]
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
  luck <- lift $ getAbility Luck
  let page' | d1 + d2 <= luck = refYes
            | otherwise = refNo
  lift $ modifyAbility (\x -> x - 1) Luck
  tell [outText "Tedd próbára SZERENCSÉDET!", OutDie d1, OutDie d2]
  evalPageItem (Goto True page')
evalPageItem (Damage ability expr) = do
  value <- evalExpr expr
  lift $ modifyAbility (\x -> x - value) ability
  return False
evalPageItem (Heal ability expr) = do
  value <- evalExpr expr
  lift $ modifyAbility (+value) ability
  return False
evalPageItem (Set flag) = lift $ setFlag flag >> return False
evalPageItem (DieDef name) = do
  n <- roll
  tell [OutDie n]
  modify (Map.insert name n)
  return False
evalPageItem (Fight enemies) = do
  is <- ask
  dice <- get
  tell [OutLink (StartFightLink enemies is dice) "Harcolj!"]
  return True
-- evalPageItem (Fight enemies) = do
--   tell [OutFight enemies $ fight `flip` enemies]
                               
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

