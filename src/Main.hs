{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Aeson ((.:))
import qualified Data.NBA.Stats as Stats
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = do
    print "enter gameID:"
    input <- getLine
    eitherErrorOrStats <- getStats input
    case eitherErrorOrStats of
        Left statsError -> print statsError
        Right stats -> mapM_ process stats

data GameStats = GameStats {
    gameID   :: String,
    teamID   :: Int,
    teamName :: String,
    teamAbv  :: String,
    teamCity :: String,
    min      :: Int,
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

process :: GameStats -> IO ()
process gs = do
  print gs
