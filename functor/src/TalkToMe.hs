module TalkToMe where

data TalkToMe a
  = Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print str x) = Print str (f x)
  fmap g (Read f) = Read (g <$> f)

instance Show a => Show (TalkToMe a) where
  show Halt = "Halt"
  show (Print x y) = x ++ show y
  show (Read f) = show $ f "memes"
