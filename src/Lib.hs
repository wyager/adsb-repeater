module Lib where

import           Options.Generic (ParseRecord, Wrapped, ParseFields, ParseField, 
                                  lispCaseModifiers, unwrapRecord, readField, parseFields,
                                  parseRecord, parseRecordWithModifiers,
                                  type (:::), type (<?>))
import           GHC.Generics    (Generic)
-- import           GHC.TypeLits   (Symbol)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL 
-- import           Data.ByteString.Lazy (ByteString)
import           System.IO (stdout)
-- import           Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
-- import qualified Data.Aeson.Types as AT
-- import qualified Data.Aeson.Diff as Diff
import           Data.Text (Text)
-- import           Data.Scientific (Scientific)
-- import qualified Data.Attoparsec.Text as AP
-- import           Control.Applicative ((<|>))
import           Control.DeepSeq (NFData, force)
import           Control.Exception (evaluate)
-- import qualified Data.Set as Set
-- import           Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as Map
-- import qualified Data.Map.Merge.Strict as Merge
import           Data.Foldable (foldlM)
import           Codec.Compression.Zstd (compress, decompress, maxCLevel)
-- import qualified Data.FingerTree as FT
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as Vector
import           Control.Concurrent.Async (concurrently)
import           Data.Hashable (Hashable)
import qualified Network.HTTP.Req as Req

data Command w 
    = Diff (w ::: FilePath <?> "Path to first file") (w ::: FilePath <?> "Path to second file")
    | Apply (w ::: FilePath <?> "Path to first file") (w ::: FilePath <?> "Path to diff")
    | Test (w ::: [FilePath] <?> "Path to files")
    | Load (w ::: FilePath <?> "Path to file")
    | LoadWeb {apiKey :: w ::: APIKey <?> "API key"}
    deriving (Generic)

instance ParseRecord (Command Wrapped) where
    parseRecord = parseRecordWithModifiers lispCaseModifiers


newtype Miscellanea = Miscellanea Aeson.Object
    deriving newtype (Aeson.ToJSON, Aeson.FromJSON, NFData)

data Parsed = Parsed {ac :: Aircrafts, miscellanea :: Miscellanea} 
    deriving (Generic)
    deriving anyclass NFData

instance Aeson.FromJSON Parsed where
    parseJSON = Aeson.withObject "Overall file" $ \object -> do
        case HM.alterF (\x -> (x, Nothing)) "ac" object of
            (Nothing,_) -> fail "Could not find field 'ac'"
            (Just aircraft, mainOther) -> flip (Aeson.withArray "ac") aircraft $ \aircraft' -> do
                let pull collected = Aeson.withObject "craft" $ \craft -> do
                        case HM.alterF (\x -> (x, Nothing)) "icao" craft of
                            (Nothing,_) -> fail "could not find field 'icao'"
                            (Just craftIcao, craftOther) -> do
                                flattened <- Aircraft <$> mapM (Aeson.withText "craft field" return) craftOther
                                Aeson.withText "icao" (insert flattened) craftIcao
                                where 
                                insert other icao = HM.alterF 
                                    (maybe (return $ Just other) (const (fail ("already present" ++ show icao)))) 
                                    (ICAO icao)
                                    collected
                pulled <- foldlM pull HM.empty aircraft'
                return (Parsed (Aircrafts pulled) (Miscellanea mainOther))

    
instance Aeson.ToJSON Parsed where
    toJSON (Parsed ac (Miscellanea mainOther)) = Aeson.Object $ HM.insert "ac" aircraft mainOther
        where
        aircraft = Aeson.Array 
                 $ Vector.fromList 
                 $ map (\(icao,craftOther) -> 
                    Aeson.Object $ HM.insert "icao" (Aeson.String $ rawICAO icao) 
                                 $ fmap Aeson.String 
                                 $ rawAircraft craftOther)
                 $ HM.toList
                 $ rawAircrafts ac

someFunc :: IO ()
someFunc = do
    command <- unwrapRecord "Flight data repeater tool"
    case command of 
        Load path -> showLoad path
        Diff a b -> showDiff a b
        Apply a db -> showApply a db
        Test paths -> test paths >>= print
        LoadWeb apiKey -> loadWeb apiKey >>= (BL.hPut stdout . Aeson.encode)

showLoad :: FilePath -> IO ()
showLoad path = do
    loaded :: Parsed <- load path
    BL.hPut stdout $ Aeson.encode loaded
    
newtype ICAO = ICAO {rawICAO :: Text}
    deriving newtype 
        ( Aeson.FromJSON, Aeson.ToJSON
        , Aeson.FromJSONKey, Aeson.ToJSONKey
        , NFData, Eq, Hashable)

newtype Aircrafts = Aircrafts {rawAircrafts :: HM.HashMap ICAO Aircraft}
    deriving newtype (Aeson.FromJSON, Aeson.ToJSON, NFData, Eq)
newtype Aircraft = Aircraft {rawAircraft :: HM.HashMap Text Text}
    deriving newtype (Aeson.FromJSON, Aeson.ToJSON, NFData, Eq)

data ACUpdate = ACDelete | ACAdd !Aircraft | ACModify !(HM.HashMap Text (Maybe Text)) 
    deriving Generic
    deriving anyclass (Aeson.ToJSON, Aeson.FromJSON, NFData)

newtype AircraftDiffs = AircraftDiffs {getAircraftDiffs :: HM.HashMap ICAO ACUpdate}
    deriving newtype (Aeson.ToJSON, Aeson.FromJSON, NFData)

data OverallDiff = OverallDiff {overallAircraftDiffs :: AircraftDiffs, overallMiscellanea :: Miscellanea}
    deriving Generic
    deriving anyclass (Aeson.ToJSON, Aeson.FromJSON, NFData)

diff :: Aircrafts -> Aircrafts -> AircraftDiffs
diff (Aircrafts a) (Aircrafts b) = AircraftDiffs $ HM.unions [delete,add,update]
    where
    delete = ACDelete <$ HM.difference a b
    add = ACAdd <$> HM.difference b a
    update = HM.mapMaybe id $ HM.intersectionWith (\l r -> if l == r then Nothing else Just $ ACModify $ diff' l r) a b
    diff' :: Aircraft -> Aircraft -> HM.HashMap Text (Maybe Text)
    diff' (Aircraft l) (Aircraft r) = HM.unions [delete',add',update']
        where
        delete' = Nothing <$ HM.difference l r
        add' = Just <$> HM.difference r l
        update' = HM.mapMaybe (Just <$>) $ HM.intersectionWith (\x y -> if x == y then Nothing else Just y) l r

load :: Aeson.FromJSON a => FilePath -> IO a
load path = either fail return . Aeson.eitherDecode' =<< BL.readFile path -- 160ms for Parsed

showDiff :: FilePath -> FilePath -> IO () 
showDiff a b = do
    (a',b') <- concurrently (load a) (load b)
    let res = diff (ac a') (ac b') -- 70ms
    BL.hPut stdout $ Aeson.encode $ OverallDiff res (miscellanea b') -- <10ms

apply :: Aircrafts -> AircraftDiffs -> Aircrafts
apply aircrafts diffs = Aircrafts $ HM.foldlWithKey' update (rawAircrafts aircrafts) (getAircraftDiffs diffs)
    where
    update acs icao = \case
        ACDelete -> HM.delete icao acs
        ACAdd ac -> HM.insert icao ac acs
        ACModify changes -> HM.adjust (apply' changes) icao acs
    apply' :: HM.HashMap Text (Maybe Text) -> Aircraft -> Aircraft
    apply' changes aircraft = Aircraft $ HM.foldlWithKey' update' (rawAircraft aircraft) changes
    update' ac key = \case
        Nothing -> HM.delete key ac
        Just val -> HM.insert key val ac


showApply :: FilePath -> FilePath -> IO ()
showApply a db = do
    a' :: Parsed <- load a
    db' :: OverallDiff <- load db
    let bac = apply (ac a') (overallAircraftDiffs db')
        b = Parsed bac (overallMiscellanea db')
    BL.hPut stdout $ Aeson.encode b
    

test :: [FilePath] -> IO (Int,Int)
test [] = return (0,0)
test (p:ps) = do
        a' :: Parsed <- load p
        go (0,0) a' ps
    where
    go (r,c) _ [] = return (fromIntegral r, c)
    go (!rawBytes,!compBytes) (prev :: Parsed) (n:ns) = do
        next :: Parsed <- load n
        let theDiff = diff (ac prev) (ac next)
            applied = apply (ac prev) theDiff
        if applied /= (ac next)
            then fail "Error: mismatch"
            else do
                let encoded = Aeson.encode theDiff
                    rawBytes' = BL.length encoded
                    compBytes' = BS.length $ compress maxCLevel $ BL.toStrict encoded
                go (rawBytes + rawBytes', compBytes + compBytes') next ns

newtype APIKey = APIKey BS.ByteString
    deriving Generic
    -- deriving anyclass (ParseRecord, ParseFields)
instance ParseFields APIKey where
    parseFields h l s = APIKey <$> parseFields h l s
instance ParseRecord APIKey where
    parseRecord = APIKey <$> parseRecord


loadWeb :: APIKey -> IO Parsed
loadWeb (APIKey key) = do
    resp <- Req.runReq Req.defaultHttpConfig $ 
        Req.req 
            Req.GET 
            (Req.https "adsbexchange.com" Req./: "api" Req./: "aircraft" Req./: "json") 
            Req.NoReqBody 
            Req.jsonResponse 
            (Req.header "api-auth" key)
    return $ Req.responseBody resp
