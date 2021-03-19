module FixerUpper where

f = const <$> Just "Hello" <*> pure "World"

g = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]
