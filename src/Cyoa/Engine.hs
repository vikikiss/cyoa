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
  
type EvaluatorT m a = RWST () [OutputItem] (Map Die Int) (CyoaT m) a                         
                         
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
          | StartFightLink [Enemy]
          | ContinueFightLink (Maybe FightRound)
                        
data Output = OutputClear String [OutputItem]
            | OutputContinue [OutputItem]
data OutputItem = OutText String
                | OutDie Int
                | OutLink Link String
                | OutBreak

evalPage :: (Functor m, MonadIO m) => CyoaT m Output
evalPage = do
  fightState <- gets fight_state
  case fightState of
    Nothing -> do
      pageNum <- gets $ player_page . player_state
      (Page _ is) <- asks (!pageNum)
      (_, w) <- execRWST `flip` () `flip` Map.empty $ do              
                     mapM_ evalPageItem is
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
      tell [OutText "Megsebzed ellenfeledet "
           ,OutText (show damage)
           ,OutText " pont értékben! "]
      hurtEnemy damage
    Just (FightRound AttackerEnemy luck) -> do
      damage <- if not luck then return 2
                  else ifLucky (return 1) (return 3)
      tell [OutText "Ellenfeled megsebez "
           ,OutText (show damage)
           ,OutText " pont értékben! "]
      -- TODO: jatekos serulese
      
  where
    ifLucky thn els = do
      d1 <- roll
      d2 <- roll
      luck <- lift $ getAbility Luck
      lift $ modifyAbility (\x -> x - 1) Luck                          
      let lucky = d1 + d2 <= luck
      tell [OutDie d1, OutDie d2, OutText $ if lucky then "Micsoda mázli!" else "Pech."]
      if lucky then thn else els

    hurtEnemy damage = do
      (e:es) <- lift $ gets $ fight_enemies . fromJust . fight_state
      let e' = e{ enemy_health = (enemy_health e) - damage }
      if enemy_health e' <= 0
        then do
          lift $ modifyFightState $ \fs -> fs{ fight_enemies = es }
          tell [ OutText $ unwords ["A(z)", enemy_name e, "holtan dől össze."] ] -- TODO: A(z)
        else 
          lift $ modifyFightState $ \fs -> fs{ fight_enemies = (e':es) }

calculateAttack :: (Functor m, MonadIO m) => Enemy -> WriterT [OutputItem] (CyoaT m) ()
calculateAttack enemy = do
  tell [OutText "Dobj a szörny nevében!"]
  enemyAttack <- rollAttack (enemy_agility enemy)
  tell [OutText "A szörny támadóereje: ", OutText (show enemyAttack)]
       
  tell [OutText "Dobj a saját támadásodért!"]
  playerAttack <- rollAttack =<< (lift $ getAbility Agility)
  tell [OutText "A Te támadóerőd: ", OutText (show playerAttack)]
  case enemyAttack `compare` playerAttack of
    EQ -> 
      tell [ OutText "Kivédtétek egymás csapását!"
           , OutLink (ContinueFightLink Nothing) "Folytasd a csatát!"]
    LT -> do
      tell [OutText "Megsebezted a teremtményt."]
      tellTryLuck AttackerPlayer
    GT -> do
      tell [OutText "Megsebez a teremtmény!"]
      tellTryLuck AttackerEnemy
        
  where tellTryLuck attacker = do
          tell [ OutText "Próbára teszed a szerencsédet?"
               , OutLink (ContinueFightLink (Just (FightRound attacker False))) "Nem."
               , OutText " "
               , OutLink (ContinueFightLink (Just (FightRound attacker True))) "Igen."
               ]
                     
fight :: (Functor m, MonadIO m) => WriterT [OutputItem] (CyoaT m) ()
fight = do
  applyLastRound
  
  enemies <- lift $ gets (fight_enemies . fromJust . fight_state)
  case enemies of
    [] -> do
      modify $ \gs -> gs{ fight_state = Nothing }
      
    (enemy:_) -> do
      -- Debug kimenet
      forM_ enemies $ \e -> do
        tell [OutText (show e), OutBreak]
      calculateAttack enemy
           
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
evalPageItem (Take item) = lift $ takeItem item
evalPageItem (Drop item) = lift $ dropItem item
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
evalPageItem (Set flag) = lift $ setFlag flag
evalPageItem (DieDef name) = do
  n <- roll
  tell [OutDie n]
  modify (Map.insert name n)
evalPageItem (Fight enemies) = do
  tell [OutLink (StartFightLink enemies) "Harcolj!"]  
-- evalPageItem (Fight enemies) = do
--   tell [OutFight enemies $ fight `flip` enemies]
                               
goto :: (MonadIO m) => Link -> CyoaT m ()
goto (PageLink pageNum) =
  modifyPlayerState $ \ps -> ps{ player_page = pageNum }  
goto (StartFightLink enemies) =
  modify $ \gs -> gs{ fight_state = Just fs }
    where fs = FS { fight_enemies = enemies,
                    fight_last_round = Nothing }
goto (ContinueFightLink round) = 
  modifyFightState (\fs -> fs{ fight_last_round = round })

