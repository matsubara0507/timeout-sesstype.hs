module Data.SessType.Utils where

import           Data.Bool (bool)

andM :: Monad m => [m Bool] -> m Bool
andM []     = pure True
andM (m:ms) = m >>= bool (pure False) (andM ms)
