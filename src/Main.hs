{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Aeson ((.:))
import Control.Exception
import System.IO.Error
import Text.Read
import Data.Function (on)
import Data.List.Split
import Data.List (sortBy, reverse)
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
    rankTeams

rankTeams :: IO ()
rankTeams = do
  atl <- openTeam "ATL"
  let atl2 = "ATL" : splitOn "\n" atl
  bkn <- openTeam "BKN"
  let bkn2 = "BKN" : splitOn "\n" bkn
  bos <- openTeam "BOS"
  let bos2 = "BOS" : splitOn "\n" bos
  cha <- openTeam "CHA"
  let cha2 = "CHA" : splitOn "\n" cha
  chi <- openTeam "CHI"
  let chi2 = "CHI" : splitOn "\n" chi
  cle <- openTeam "CLE"
  let cle2 = "CLE" : splitOn "\n" cle
  dal <- openTeam "DAL"
  let dal2 = "DAL" : splitOn "\n" dal
  den <- openTeam "DEN"
  let den2 = "DEN" : splitOn "\n" den
  det <- openTeam "DET"
  let det2 = "DET" : splitOn "\n" det
  gsw <- openTeam "GSW"
  let gsw2 = "GSW" : splitOn "\n" gsw
  hou <- openTeam "HOU"
  let hou2 = "HOU" : splitOn "\n" hou
  ind <- openTeam "IND"
  let ind2 = "IND" : splitOn "\n" ind
  lac <- openTeam "LAC"
  let lac2 = "LAC" : splitOn "\n" lac
  lal <- openTeam "LAL"
  let lal2 = "LAL" : splitOn "\n" lal
  mem <- openTeam "MEM"
  let mem2 = "MEM" : splitOn "\n" mem
  mia <- openTeam "MIA"
  let mia2 = "MIA" : splitOn "\n" mia
  mil <- openTeam "MIL"
  let mil2 = "MIL" : splitOn "\n" mil
  min <- openTeam "MIN"
  let min2 = "MIN" : splitOn "\n" min
  nop <- openTeam "NOP"
  let nop2 = "NOP" : splitOn "\n" nop
  nyk <- openTeam "NYK"
  let nyk2 = "NYK" : splitOn "\n" nyk
  okc <- openTeam "OKC"
  let okc2 = "OKC" : splitOn "\n" okc
  orl <- openTeam "ORL"
  let orl2 = "ORL" : splitOn "\n" orl
  phi <- openTeam "PHI"
  let phi2 = "PHI" : splitOn "\n" phi
  phx <- openTeam "PHX"
  let phx2 = "PHX" : splitOn "\n" phx
  por <- openTeam "POR"
  let por2 = "POR" : splitOn "\n" por
  sac <- openTeam "SAC"
  let sac2 = "SAC" : splitOn "\n" sac
  sas <- openTeam "SAS"
  let sas2 = "SAS" : splitOn "\n" sas
  tor <- openTeam "TOR"
  let tor2 = "TOR" : splitOn "\n" tor
  uta <- openTeam "UTA"
  let uta2 = "UTA" : splitOn "\n" uta
  was <- openTeam "WAS"
  let was2 = "WAS" : splitOn "\n" was

  let scores = [atl2, bkn2, bos2, cha2, chi2, cle2, dal2, den2, det2, gsw2, hou2, ind2, lac2, lal2, mem2, mia2, mil2, min2, nop2, nyk2, okc2, orl2, phi2, phx2, por2, sac2, sas2, tor2, uta2, was2]
  let scores2 = map calcRanks scores
  let scores3 = reverse $ sortBy (compare `on` snd) (scores2)
  let scores4 = map combineScore scores3
  mapM_ putStrLn scores4

calcRanks :: [String] -> (String, Double)
calcRanks a = ((a!!0),((read (a!!1))+(read (a!!2))+(read (a!!3))))

combineScore :: (String, Double) -> String
combineScore (s, d) = s ++ ": " ++ show d

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
      --print $ minutes gs
      t <- makeTeam gs gsopp
      --print gsopp
      print "Creating Team..."
    else do
      let newScore = (calcScore (read ((lines t)!!0) :: Double) gs) ++ "\n" ++ (calcOff (read ((lines t)!!1) :: Double) gs gsopp ++ "\n" ++ calcDef (read ((lines t)!!2) :: Double) gs gsopp)
      t <- update gs newScore
      --print gsopp
      print $ (teamName gs) ++ " Stats Added"
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
