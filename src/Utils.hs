module Utils where

ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM pred a b = do bool <- pred
                  if (bool) then a else b
