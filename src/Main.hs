{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad (forM_)
import Control.Applicative ( (<$>) )
import Data.Char (toLower)
import Data.List (union)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding -- (decodeUtf8)

import qualified Data.HashSet as HS
import qualified Data.Map.Strict as M

import qualified Data.Aeson as A (decode', encode)
import Data.Aeson.TH

import           Data.CaseInsensitive ( CI )
import qualified Data.CaseInsensitive as CI

data Tweet = Tweet
    { tTid   :: {-# UNPACK #-}!T.Text
    , tIndex :: {-# UNPACK #-}!T.Text
    , tInput :: [T.Text]
    , tOutput :: Maybe [T.Text]
    } deriving (Eq, Show)

deriveJSON defaultOptions{fieldLabelModifier = fmap toLower . drop 1} ''Tweet

-- newtype CorrectionDictionary = CorrectionDictionary (M.Map T.Text [T.Text])
--     deriving (Eq, Show, Functor)

getTweets :: FilePath -> IO (Maybe [Tweet])
getTweets path = A.decode' <$> BL.readFile path

loadDictionary :: FilePath -> IO (HS.HashSet (CI T.Text))
loadDictionary path = HS.fromList . fmap (CI.mk . TL.toStrict) . TL.lines . decodeLatin1 <$> BL.readFile path

emnlpCorrectionDictionary :: FilePath -> IO (M.Map (CI T.Text) [T.Text])
emnlpCorrectionDictionary path = M.fromList . fmap (toPairs . T.words . TL.toStrict) . TL.lines . decodeUtf8 <$> BL.readFile path
    where toPairs (x:xs) = (CI.mk x, xs)

utdallasCorrectionDictionary :: FilePath -> IO (M.Map (CI T.Text) [T.Text])
utdallasCorrectionDictionary path = M.fromList . fmap (toPairs . T.words . TL.toStrict) . TL.lines . decodeUtf8 <$> BL.readFile path
    where
        toPairs :: [T.Text] -> (CI T.Text, [T.Text])
        toPairs (_:word:xs) = (CI.mk word, filter (/= "|") xs)

unionCorrDicts :: M.Map (CI T.Text) [T.Text] -> M.Map (CI T.Text) [T.Text] -> M.Map (CI T.Text) [T.Text]
unionCorrDicts = M.unionWith Data.List.union

main = do
    Just tweets <- getTweets "data/test_data_20150430.json"
    dict <- loadDictionary "data/scowl.american.70"

    emnlp <- emnlpCorrectionDictionary "data/emnlp_dict.txt"
    utdallas <- utdallasCorrectionDictionary "data/utdallas.txt"

    let correctionDictionary = unionCorrDicts emnlp utdallas

    let check x = HS.member (CI.mk x) dict
    let checkCorrection x = M.lookup (CI.mk x) correctionDictionary

    -- let first = head tweets
    -- print first
    let result = fmap (fmap (\x -> (x, check x, checkCorrection x)) . tInput) tweets
    forM_ result $ \x -> print x >> putStrLn ""

    BL.writeFile "output.json" $ A.encode result

-- import Data.Binary (decodeFile)
-- import Codec.Archive.Zip

-- mapZip :: (BL.ByteString -> b) -> FilePath -> IO [b]
-- mapZip f path = map (f . fromEntry) . zEntries <$> decodeFile path
