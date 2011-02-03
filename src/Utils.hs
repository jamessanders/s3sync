module Utils where

ifM pred a b = do bool <- pred
                  if (bool) then a else b
