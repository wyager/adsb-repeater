module Lib where

import           Options.Generic (ParseRecord, Wrapped, 
                                  lispCaseModifiers, unwrapRecord,
                                  parseRecord, parseRecordWithModifiers,
                                  type (:::), type (<?>))
import           GHC.Generics    (Generic)
import           GHC.TypeLits   (Symbol)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL 
import           Data.ByteString.Lazy (ByteString)
import           System.IO (stdout)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Diff as Diff
import           Data.Text (Text)
import           Data.Scientific (Scientific)
import qualified Data.Attoparsec.Text as AP
import           Control.Applicative ((<|>))
import           Control.DeepSeq (NFData, force, rnf)
import           Control.Exception (evaluate)
import qualified Data.Set as Set
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Merge
import           Data.Foldable (toList)
import           Codec.Compression.Zstd (compress, decompress, maxCLevel)

-- import           Lens.Micro ((^.))
-- import           Data.Generics.Product (field, HasField')
-- import           Data.Proxy (Proxy(..))

data Command w 
    = Count (w ::: [FilePath] <?> "Path to files")
    | Diffs (w ::: [FilePath] <?> "Path to files")
    | Diff (w ::: FilePath <?> "Path to first file") (w ::: FilePath <?> "Path to second file")
    | Apply (w ::: FilePath <?> "Path to first file") (w ::: FilePath <?> "Path to diff")
    | Test (w ::: [FilePath] <?> "Path to files")
    deriving (Generic)

instance ParseRecord (Command Wrapped) where
    parseRecord = parseRecordWithModifiers lispCaseModifiers

someFunc :: IO ()
someFunc = do
    command <- unwrapRecord "Flight data repeater tool"
    case command of 
        Count files -> mapM_ count files
        Diffs files -> diffs files
        Diff a b -> diff a b
        Apply a db -> apply a db
        Test paths -> test paths >>= print

count :: FilePath  -> IO ()
count first = do
    first' :: Contents [RawEntry] <- either fail (evaluate . force) . Aeson.eitherDecode' =<< BL.readFile first
    print (length $ ac first')
    print (length $ Set.fromList $ map _icao $ ac first')

load :: (Aeson.FromJSON a, NFData a) => FilePath -> IO (Contents a)
load file = either fail (evaluate . force) . Aeson.eitherDecode' =<< BL.readFile file 

diffs :: [FilePath] -> IO ()
diffs files  = do
    contents :: [Contents [RawEntry]] <- mapM load files 
    let collected = map collect contents
    let diffs = zipWith diffContents collected (tail collected)
    mapM_ (BL.hPut stdout . Aeson.encode) diffs

diff :: FilePath -> FilePath ->IO ()
diff a b = do
    a' :: Contents [RawEntry] <- load a
    b' :: Contents [RawEntry] <- load b
    BL.hPut stdout $ Aeson.encode $ diffContents (collect a') (collect b')

apply :: FilePath -> FilePath -> IO ()
apply a db = do
    a' :: Contents [RawEntry] <- load a
    db' :: Contents (Map Text Operation) <- load db
    let b = applyContents (collect a') db'
    b' :: Contents [RawEntry] <- either fail (return . fmap toList) b
    BL.hPut stdout $ Aeson.encode b'

test :: [FilePath] -> IO (Int,Int)
test [] = return (0,0)
test (p:ps) = do
        a' :: Contents [RawEntry] <- load p
        go (0,0) (collect a') ps
    where
    go (r,c) _ [] = return (fromIntegral r, c)
    go (!rawBytes,!compBytes) (prev :: Contents (Map Text RawEntry)) (n:ns) = do
        next :: Contents [RawEntry] <- load n
        let collected = collect next
            diff = diffContents prev collected
        applied <- either fail return $ applyContents prev diff
        if applied /= collected
            then fail "Error: mismatch"
            else do
                let encoded = Aeson.encode diff
                    rawBytes' = BL.length encoded
                    compBytes' = BS.length $ compress 19 $ BL.toStrict encoded
                go (rawBytes + rawBytes', compBytes + compBytes') collected ns

diffRaw :: RawEntry -> RawEntry -> Diff.Patch
diffRaw a b = Diff.diff (Aeson.toJSON a) (Aeson.toJSON b)

applyDiffRaw :: RawEntry -> Diff.Patch -> Either String RawEntry
applyDiffRaw a patch = do
    let result = do
            patched <- Diff.patch patch (Aeson.toJSON a) 
            Aeson.fromJSON patched
    case result of
        Aeson.Error e -> Left e
        Aeson.Success a -> Right a


data RawEntry = RawEntry 
    { _postime :: !Text
    , _icao :: !Text
    , _reg :: !Text
    , _type :: !Text
    , _wtc :: !Text
    , _spd :: !Text
    , _altt :: !Text
    , _alt :: !Text
    , _galt :: !Text
    , _talt :: !Text
    , _lat :: !Text
    , _lon :: !Text
    , _vsit :: !Text
    , _vsi :: !Text
    , _trkh :: !Text
    , _ttrk :: !Text
    , _trak :: !Text
    , _sqk :: !Text
    , _call :: !Text
    , _gnd :: !Text
    , _trt :: !Text
    , _pos :: !Text
    , _mlat :: !Text
    , _tisb :: !Text
    , _sat :: !Text
    , _opicao :: !Text
    , _cou :: !Text
    , _mil :: !Text
    , _interested :: Text
    } 
    deriving stock (Eq, Generic, Show)
    deriving anyclass (NFData)
options :: Aeson.Options
options = Aeson.defaultOptions {Aeson.fieldLabelModifier = tail}
instance Aeson.ToJSON RawEntry where
    toEncoding = Aeson.genericToEncoding options
    toJSON = Aeson.genericToJSON options
instance Aeson.FromJSON RawEntry where
    parseJSON = Aeson.genericParseJSON options 

data Entry = Entry
    { postime :: !Word
    , icao :: !Word -- hex
    , reg :: !Text
    -- , _type :: Text
    -- , _wtc :: Text
    , spd :: !(Maybe Double)
    -- , _altt :: Text
    -- , _alt :: Text
    -- , _galt :: Text
    -- , _talt :: Text
    -- , _lat :: Text
    -- , _lon :: Text
    -- , _vsit :: Text
    -- , _vsi :: Text
    -- , _trkh :: Text
    -- , _ttrk :: Text
    -- , _trak :: Text
    -- , _sqk :: Text
    -- , _call :: Text
    -- , _gnd :: Text
    -- , _trt :: Text
    -- , _pos :: Text
    -- , _mlat :: Text
    -- , _tisb :: Text
    -- , _sat :: Text
    -- , _opicao :: Text
    , cou :: !Text
    , mil :: !Bool -- 0/1
    -- , _interested :: Text
    } 
    deriving (Generic)
    deriving anyclass (NFData, Aeson.ToJSON)

-- entry :: RawEntry -> Either String Entry
-- entry raw = do
--     Entry 
--         <$> AP.parseOnly AP.decimal (_postime raw)
--         <*> AP.parseOnly AP.hexadecimal (_icao raw)
--         <*> return (_reg raw)
--         <*> AP.parseOnly ((Just <$> AP.double) <|> (Nothing <$ "")) (_spd raw)
--         <*> return (_cou raw)
--         <*> AP.parseOnly ((True <$ "1") <|> (False <$ "0")) (_mil raw)
    -- Entry <$>   <*> hex _icao <*> return (_reg raw) <*> dbl _spd <*> return (_cou raw) <*> bool _mil

-- data FailableEntry 
--     = ParsedEntry Entry 
--     | UnparsedEntry RawEntry
--     deriving (Generic)
--     deriving anyclass (NFData, Aeson.ToJSON)

-- instance Aeson.FromJSON FailableEntry where
--     parseJSON val = do
--         raw <- Aeson.parseJSON val
--         case entry raw of
--             Left _err -> return (UnparsedEntry raw)
--             Right parsed -> return (ParsedEntry parsed)

data Contents entries = Contents 
    { ac :: entries
    , msg :: Text
    , total :: Word
    , ctime :: Word
    , ptime :: Word
    } deriving (Generic, Functor, Eq)
    deriving anyclass (Aeson.FromJSON, Aeson.ToJSON, NFData)

data Operation = Delete | Add RawEntry | Patch Diff.Patch 
    deriving (Generic, Show)
    deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)
instance NFData Operation where
    rnf Delete = ()
    rnf (Add e) = rnf e
    rnf (Patch !p) = ()

diffEntries :: Map Text RawEntry -> Map Text RawEntry -> Map Text Operation
diffEntries = 
    Merge.merge 
        (Delete <$ Merge.preserveMissing)
        (Add <$> Merge.preserveMissing)
        (Merge.zipWithMaybeMatched (\_k l r -> if l == r then Nothing else Just (Patch (diffRaw l r))))

applyEntries :: Map Text RawEntry -> Map Text Operation -> Either String (Map Text RawEntry)
applyEntries =
    Merge.mergeA
        (Merge.preserveMissing) -- No patch on this one
        (Merge.traverseMissing (\k -> \case
            Add b -> return b
            Patch _p -> Left ("Cannot patch unknown key: " ++ show k)
            Delete -> Left ("Cannot delete unknown key: " ++ show k)))
        (Merge.zipWithMaybeAMatched (\_ a db -> case db of
            Patch patch -> Just <$> applyDiffRaw a patch
            Add b -> Left ("Attempting to overwrite existing: " ++ show b)
            Delete -> return Nothing))

collect :: Contents [RawEntry] -> Contents (Map Text RawEntry)
collect = fmap (Map.fromList . map (\entry -> (_icao entry, entry)))

diffContents :: Contents (Map Text RawEntry) -> Contents (Map Text RawEntry) -> Contents (Map Text Operation)
diffContents a b = fmap (diffEntries (ac a)) b

applyContents :: Contents (Map Text RawEntry) -> Contents (Map Text Operation) -> Either String (Contents (Map Text RawEntry))
applyContents a db = do
    entries <- applyEntries (ac a) (ac db)
    return $ db {ac = entries}

