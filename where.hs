module Function where

  withWhere n = print plusTwo
    where plusTwo = n + 2

  withLet n = let plusTwo = n + 2
    in print plusTwo