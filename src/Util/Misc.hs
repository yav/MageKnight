module Util.Misc where

repeatN :: Monad m => Int -> (a -> m a) -> (a -> m a)
repeatN n step s = if n > 0 then step s >>= repeatN (n-1) step else return s
