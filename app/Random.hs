module Random where


import Control.Monad.State (MonadState(get, put))
import System.Random (RandomGen, Random(randomR))


chooseRandomM :: MonadState g m => RandomGen g => [a] -> m a
chooseRandomM ls = do
  let n = length ls
  i <- randomRM (0, n - 1)
  pure $ ls !! i


randomRM :: MonadState g m => RandomGen g => Random a => (a, a) -> m a
randomRM r = do
  s <- get
  let (x, newS) = randomR r s
  put newS
  pure x
