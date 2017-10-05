{-# LANGUAGE OverloadedStrings #-}
module Commands where


import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8 (Parser, string, space, decimal, many1, letter_ascii, sepBy, char, digit, digit, option)
import Data.Char (ord)
import Data.Text (Text, pack)


data Command
  = SettingsCommand Setting
  | UpdateCommand Update
  | ActionCommand Action
  | EmptyCommand
  deriving (Show)


data Setting
  = Timebank Int
  | TimePerMove Int
  | PlayerNames [Text]
  | YourBot Text
  | YourBotId Int
  | FieldWidth Int
  | FieldHeight Int
  | MaxRounds Int
  deriving (Show)


data Update
  = GameRound Int
  | GameField [[FieldItem]]
  | PlayerSnippets Text Int
  | PlayerBombs Text Int
  deriving (Show)


data Action
  = Move Int
  | Character Int
  deriving (Show)


data FieldItem
  = Empty
  | Wall
  | Player Int
  | BugSpawn (Maybe Int)
  | Gate GateDirection
  | Bug BugAIType
  | Mine (Maybe Int)
  | Snippet
  deriving (Show, Eq)


data GateDirection = GdLeft
                   | GdRight
     deriving (Show, Eq)


data BugAIType = Chase
               | Predict
               | Lever
               | FarChase
     deriving (Show, Eq, Enum)


data MoveAction = ChoosingCharacter CharacterType
                | Moving MoveDirection (Maybe Int)
     deriving (Eq)


data CharacterType = Bixie
                   | Bixiette
     deriving (Eq)


data MoveDirection
  = MoveLeft
  | MoveRight
  | MoveUp
  | MoveDown
  | MovePass
  deriving (Eq, Ord)


instance Show MoveDirection where
  show MoveLeft = "left"
  show MoveRight = "right"
  show MoveUp = "up"
  show MoveDown = "down"
  show MovePass = "no_moves"


instance Show CharacterType where
  show Bixie = "bixie"
  show Bixiette = "bixiette"


instance Show MoveAction where
  show (ChoosingCharacter c) = show c
  show (Moving dir mb) =
    let b =
          case mb of
            Nothing -> ""
            Just i -> ";drop_bomb " ++ show i
    in show dir ++ b


command :: Parser Command
command =
  settingsCommand <|> updateCommand <|> actionCommand <|> emptyCommand
  where
    settingsCommand =
      SettingsCommand <$> (string "settings" *> space *> setting)
    updateCommand =
      UpdateCommand <$> (string "update" *> space *> update)
    actionCommand =
      ActionCommand <$> (string "action" *> space *> action)
    setting =
      timebank <|> timePerMove <|> playerNames <|> yourBot <|> yourBotId <|> fieldWidth <|> fieldHeight <|> maxRounds
    update =
      gameRound <|> gameField <|> playerSnippets <|> playerBombs
    action =
      moveAction <|> characterAction
    timebank =
      Timebank <$> (string "timebank" *> space *> decimal)
    timePerMove =
      TimePerMove <$> (string "time_per_move" *> space *> decimal)
    playerNames =
      PlayerNames <$> (string "player_names" *> space *> playerNamesList)
    yourBot =
      YourBot <$> (string "your_bot" *> space *> playerName)
    yourBotId =
      YourBotId <$> (string "your_botid" *> space *> decimal)
    fieldWidth =
      FieldWidth <$> (string "field_width" *> space *> decimal)
    fieldHeight =
      FieldHeight <$> (string "field_height" *> space *> decimal)
    maxRounds =
      MaxRounds <$> (string "max_rounds" *> space *> decimal)
    gameRound =
      GameRound <$> (string "game round " *> decimal)
    gameField =
      GameField <$> (string "game field " *> (gameFieldItems `sepBy` char ','))
    playerSnippets = do
      p <- playerName
      _ <- space *> string "snippets" *> space
      i <- decimal
      pure $ PlayerSnippets p i
    playerBombs = do
      p <- playerName
      _ <- space *> string "bombs" *> space
      i <- decimal
      pure $ PlayerBombs p i
    playerNamesList =
      playerName `sepBy` char ','
    playerName =
      pack <$> many1 (letter_ascii <|> digit)
    gameFieldItems =
      gameFieldItem `sepBy` char ';'
    gameFieldItem =
      fiEmpty <|> fiWall <|> fiPlayer <|> fiBugSpawn <|> fiGate <|> fiBug <|> fiMine <|> fiSnippet
    fiEmpty =
      char '.' *> return Empty
    fiWall =
      char 'x' *> return Wall
    fiPlayer = do
      _ <- char 'P'
      d <- toInt <$> digit
      pure $ Player d
    fiBugSpawn = do
      _ <- char 'S'
      md <- option Nothing (Just <$> digit)
      pure $ BugSpawn (toInt <$> md)
    fiGate =
      Gate <$> (char 'G' *> gateDirection)
    fiBug =
      Bug <$> (char 'E' *> bugType)
    fiMine =
      Mine <$> (char 'B' *> optionalMineTimer)
    fiSnippet =
      char 'C' *> pure Snippet
    toInt c =
      ord c - ord '0'
    gateDirection =
      gdLeft <|> gdRight
    gdLeft =
      char 'l' *> pure GdLeft
    gdRight =
      char 'r' *> pure GdRight
    bugType = do
      d <- toInt <$> digit
      pure $ toEnum d
    optionalMineTimer = do
      md <- option Nothing (Just <$> digit)
      pure (toInt <$> md)
    moveAction =
      Move <$> (string "move" *> space *> decimal)
    characterAction =
      Character <$> (string "character" *> space *> decimal)
    emptyCommand =
      string "" *> return EmptyCommand
