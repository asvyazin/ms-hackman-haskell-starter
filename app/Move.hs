module Move where


import Control.Monad.State (MonadState)
import Commands (Action(..), MoveDirection(..), MoveAction(..), CharacterType(..))
import Game (Game(..), GameState(..), GameSettings(..), getNeighbours, getPlayerPosition)
import Random (chooseRandomM)
import System.Random (RandomGen)


action :: MonadState g m => RandomGen g => Game -> Action -> m MoveAction
action _ (Character _) = do
  c <- chooseRandomM [Bixie, Bixiette]
  pure $ ChoosingCharacter c
action game (Move _) = do
  (d, i) <- move game
  pure $ Moving d i


move :: MonadState g m => RandomGen g => Game -> m (MoveDirection, Maybe Int)
move game = do
  let field =
        stateField $ gameState game
      yourBotId =
        settingsYourBotId $ gameSettings game
      yourPos =
        getPlayerPosition yourBotId field
      neighbours =
        getNeighbours field yourPos
  case neighbours of
    [] -> pure (MovePass, Nothing)
    _ -> do
      (m, _) <- chooseRandomM neighbours
      pure (m, Nothing)
