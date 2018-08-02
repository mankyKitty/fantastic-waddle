module Internal 
  ( (<$$)
  ) where
    
(<$$) :: (Functor f, Functor g) => a -> f (g b) -> f (g a)
(<$$) a = fmap (a <$)

infixr 9 <$$