{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Aeson ((.:))
import Control.Exception
import System.IO.Error
import Text.Read
import Data.List.Split
import qualified Data.NBA.Stats as Stats
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Char8 as C
import qualified System.IO.Strict as S

main :: IO ()
main = do
    print "enter gameID:"
    input <- getLine
    print "enter number of games to be processed:"
    ngamesio <- getLine
    let ngames = read ngamesio :: Int
    statGames ngames input


statGames :: Int -> String -> IO ()
statGames 0 id = return ()
statGames i id = do
    eitherErrorOrStats <- getStats id
    case eitherErrorOrStats of
        Left statsError -> print statsError
        Right stats -> mapM_ (process stats) stats
    statGames (i-1) ("00" ++ show (read id + 1)) 

data GameStats = GameStats {
    gameID   :: String,
    teamID   :: Int,
    teamName :: String,
    teamAbv  :: String,
    teamCity :: String,
    minutes      :: String,
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
        minutes <- o .: "MIN"
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

calcOff :: Double -> GameStats -> GameStats -> String
calcOff 0.0 gs gsopp = show (pts gs)
calcOff prev gs gsopp = do
  let tmpoints = fromIntegral $ pts gs
  let tmfga = fromIntegral $ fga gs :: Double
  let tmfta = fromIntegral $ fta gs :: Double
  let tmorb = fromIntegral $ oreb gs :: Double
  let tmfgm = fromIntegral $ fgm gs :: Double
  let tmtov = fromIntegral $ to gs :: Double
  let tmdrb = fromIntegral $ reb gs :: Double
  let opdrb = fromIntegral $ reb gsopp :: Double
  let opfga = fromIntegral $ fga gsopp :: Double
  let opfta = fromIntegral $ fta gsopp :: Double
  let oporb = fromIntegral $ oreb gsopp :: Double
  let opfgm = fromIntegral $ fgm gsopp :: Double
  let optov = fromIntegral $ to gsopp :: Double

  let result = 100 * (tmpoints / (0.5*((tmfga+0.4*tmfta-1.07*(tmorb/(tmorb+opdrb))*(tmfga-tmfgm)+tmtov) + (opfga + 0.4*opfta - 1.07*(oporb/(oporb+tmdrb))*(opfga-opfgm)+optov))))

  show $ (result + prev)/2.0

calcDef :: Double -> GameStats -> GameStats -> String
calcDef 0.0 gs gsopp = show (pts gs)
calcDef prev gs gsopp = do
  let opfga = fromIntegral $ fga gsopp :: Double
  let opfgm = fromIntegral $ fgm gsopp :: Double
  let tmblk = fromIntegral $ blk gsopp :: Double
  let tmmp  = fromIntegral $ read ((splitOn ":" (minutes gs))!!0) :: Double
  let oporb = fromIntegral $ oreb gsopp :: Double
  let tmdrb = fromIntegral $ reb gs    :: Double
  let optov = fromIntegral $ to gsopp  :: Double
  let tmstl = fromIntegral $ stl gs    :: Double
  let tmpf  = fromIntegral $ pf gs     :: Double
  let totpf = tmpf + (fromIntegral (pf gsopp) :: Double)
  let opfta = fromIntegral $ fta gsopp :: Double
  let opftm = fromIntegral $ ftm gsopp :: Double
  let dor = oporb/(oporb+tmdrb)
  let dfg = opfgm/opfga
  let fmwt = (dfg*(1-dor))/(dfg*(1-dor)+(1-dfg)*dor)
  let stops1 = tmstl + tmblk * fmwt * (1-1.07*dor) + tmdrb*(1-fmwt)
  let stops2 = (((opfga - opfgm - tmblk)/tmmp)*fmwt*(1-1.07*dor)+((optov - tmstl)/tmmp))*tmmp + (totpf/tmpf)*0.4*opfta*(1-(opftm/opfta))
  let stops3 = stops2*stops2

  let result = stops1+stops2
  show $ (result + prev)/2.0

process :: [GameStats] -> GameStats -> IO ()
process []        gs = return ()
process (gsopp:gss) gs
  | (gs==gsopp) = process gss gs
  | otherwise   = do
    t <- openTeam (teamAbv gs)
    if (t == "Error") then do
      print $ minutes gs
      t <- makeTeam gs gsopp
      print gsopp
    else do
      let newScore = (calcScore (read ((lines t)!!0) :: Double) gs) ++ "\n" ++ (calcOff (read ((lines t)!!1) :: Double) gs gsopp ++ "\n" ++ calcDef (read ((lines t)!!2) :: Double) gs gsopp)
      t <- update gs newScore
      print gsopp
    process gss gs

update :: GameStats -> String -> IO ()
update gs score = do
  let fn = "data/teams/" ++ (teamAbv gs) ++ ".txt"
  writeFile fn score
  

makeTeam :: GameStats -> GameStats -> IO ()
makeTeam gs gsopp = do
  let writeData = calcScore 0 gs ++ "\n" ++ (calcOff 100 gs gsopp) ++ "\n" ++ (calcDef 100 gs gsopp)
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
