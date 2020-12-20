f ::
  (a, b, c) ->
  (d, e, f) ->
  ((a, d), (c, f))
f (a, _, c) (d, _, f) = ((a, d), (c, f))

nums x =
  case compare x 0 of
    LT -> -1
    EQ -> 0
    GT -> 1