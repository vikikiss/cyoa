module Cyoa.PageLang where

type Item = String
type Flag = String
type Counter = String
type Die = String -- Dobokocka

data Stat = Health -- Eletero
          | Agility -- Ugyesseg
          | Luck -- Szerencse
          deriving (Eq, Ord, Show, Read)

type PageNum = Int

data Expr = ELiteral Int
          | Expr :+: Expr
          | Expr :-: Expr
          | Expr :*: Expr
          | Expr :%: Expr
          | DieRef Die
          | ECond Cond Expr Expr -- ?:
          | CounterRef Counter
          | StatQuery Stat
          deriving Show

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

data PageType = NormalPage
              | WinPage
              | DeathPage
              deriving Show
                   
data Page = Page PageNum PageType [PageItem]
          deriving Show
                   
data PageItem = Paragraph [PageItem]
              | TextLit String
              | If Cond [PageItem] [PageItem]
              | Goto Bool PageNum
              | GotoLucky PageNum PageNum
              | DieDef Die
              | Heal Stat Expr
              | Damage Stat Expr
              | Take Item
              | Drop Item
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
