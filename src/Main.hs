{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Aeson ((.:))
import Control.Exception
import System.IO.Error
import Text.Read
import qualified Data.NBA.Stats as Stats
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Char8 as C
import qualified System.IO.Strict as S

main :: IO ()
main = do
    print "enter gameID:"
    input <- getLine
    print "enter number of games today:"
    ngamesio <- getLine
    let ngames = read ngamesio :: Int
    statGames ngames input

statGames :: Int -> String -> IO ()
statGames 0 id = return ()
statGames i id = do
    eitherErrorOrStats <- getStats id
    case eitherErrorOrStats of
        Left statsError -> print statsError
        Right stats -> mapM_ process stats
    statGames (i-1) ("00" ++ show (read id + 1)) 

data GameStats = GameStats {
    gameID   :: String,
    teamID   :: Int,
    teamName :: String,
    teamAbv  :: String,
    teamCity :: String,
    min      :: String,
    fgm      :: Int,
    fga      :: Int,
    fgp      :: Double,
    fg3m     :: Int,
    fg3a     :: Int,
    fg3p     :: Double,
    ftm      :: Int,
    fta      :: Int,
    ftp      :: Double,
    oreb     :: Int,
    dreb     :: Int,
    reb      :: Int,
    ast      :: Int,
    stl      :: Int,
    blk      :: Int,
    to       :: Int,
    pf       :: Int,
    pts      :: Int,
    plusmin  :: Double
} deriving (Show, Eq)

instance Aeson.FromJSON GameStats where
    parseJSON (Aeson.Object o) = do
        gameID <- o .: "GAME_ID"
        teamID <- o .: "TEAM_ID"
        teamName <- o .: "TEAM_NAME"
        teamAbv <- o .: "TEAM_ABBREVIATION"
        teamCity <- o .: "TEAM_CITY"
        min <- o .: "MIN"
        fgm <- o .: "FGM"
        fga <- o .: "FGA"
        fgp <- o .: "FG_PCT"
        fg3m <- o .: "FG3M"
        fg3a <- o .: "FG3A"
        fg3p <- o .: "FG3_PCT"
        ftm <- o .: "FTM"
        fta <- o .: "FTA"
        ftp <- o .: "FT_PCT"
        oreb <- o .: "OREB"
        dreb <- o .: "DREB"
        reb <- o .: "REB"
        ast <- o .: "AST"
        stl <- o .: "STL"
        blk <- o .: "BLK"
        to <- o .: "TO"
        pf <- o .: "PF"
        pts <- o .: "PTS"
        plusmin <- o .: "PLUS_MINUS"
        return GameStats {..}
    parseJSON invalid = Aeson.typeMismatch "GameStats" invalid

getStats :: String -> IO (Either Stats.StatsError [GameStats])
getStats id = Stats.getSplitRows "boxscoretraditionalv2" "TeamStats"
  [
      ("EndPeriod", Just "10"),
      ("EndRange", Just "28800"),
      ("GameID", Just (C.pack id)),
      ("RangeType", Just "0"),
      ("StartPeriod", Just "1"),
      ("StartRange", Just "0")
  ]

calcScore :: Double -> GameStats -> String
calcScore 0.0 gs = show (pts gs)
calcScore prev gs = show $ ((fromIntegral (pts gs)) + prev)/2.0



process :: GameStats -> IO ()
process gs = do
  t <- openTeam (teamAbv gs)
  if (t == "Error") then do
    t <- makeTeam gs
    print t
  else do
    let newScore = calcScore (read t :: Double) gs
    t <- update gs newScore
    print t

update :: GameStats -> String -> IO ()
update gs score = do
  let fn = "data/teams/" ++ (teamAbv gs) ++ ".txt"
  writeFile fn score
  

makeTeam :: GameStats -> IO ()
makeTeam gs = do
  let writeData = calcScore 0 gs
  let fn = "data/teams/" ++ (teamAbv gs) ++ ".txt"
  writeFile fn writeData

openTeam :: String -> IO (String)
openTeam n = do
  let fn = "data/teams/" ++ n ++ ".txt"
  contents <- tryJust handleReadFile (S.readFile fn)
  case contents of
    Left except -> return $ "Error"
    Right cont -> return cont
  where
    handleReadFile :: IOError -> Maybe String
    handleReadFile er
      | isDoesNotExistError er = Just "team does not exist"
      | isPermissionError   er = Just "permission denied"
      | otherwise              = Nothing 
