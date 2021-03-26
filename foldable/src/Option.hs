module Option where

data Option a
  = Some a
  | None

instance Foldable Option where
  foldr _ z None = z
  foldr f z (Some x) = f x z

  foldl _ z None = z
  foldl f z (Some x) = f z x

  foldMap _ None = mempty
  foldMap f (Some x) = f x
