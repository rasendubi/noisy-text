{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad (forM_)
import Control.Applicative ( (<$>), (<*>) )
import Data.Char (toLower)
import Data.List (union)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8, decodeLatin1)

import qualified Data.HashSet as HS
import qualified Data.Map.Strict as M

import qualified Data.Aeson as JSON (decode', encode)
import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)

import           Data.CaseInsensitive ( CI )
import qualified Data.CaseInsensitive as CI

data Tweet = Tweet
    { tTid    :: {-# UNPACK #-}!T.Text
    , tIndex  :: {-# UNPACK #-}!T.Text
    , tInput  :: ![T.Text]
    , tOutput :: !(Maybe [T.Text])
    } deriving (Eq, Show)

deriveJSON defaultOptions{fieldLabelModifier = fmap toLower . drop 1} ''Tweet

type Vocabulary = HS.HashSet (CI T.Text)
type CorrectionDictionary = M.Map (CI T.Text) [T.Text]

main = do
    Just tweets <- getTweets "data/test_data_20150430.json"
    vocabulary <- loadVocabulary "data/scowl.american.70"
    correctionDictionary <- buildCorrectionDictionary

    let checkOOV x = HS.member (CI.mk x) vocabulary
    let checkCorrection x = M.lookup (CI.mk x) correctionDictionary

    let result = fmap (fmap (\x -> (x, checkOOV x, checkCorrection x)) . tInput) tweets
    forM_ result $ \x -> print x >> putStrLn ""

    BL.writeFile "output.json" $ JSON.encode result

getTweets :: FilePath -> IO (Maybe [Tweet])
getTweets path = JSON.decode' <$> BL.readFile path

loadVocabulary :: FilePath -> IO Vocabulary
loadVocabulary path = HS.fromList . fmap (CI.mk . TL.toStrict) . TL.lines . decodeLatin1 <$> BL.readFile path

buildCorrectionDictionary :: IO CorrectionDictionary
buildCorrectionDictionary = M.unionWith Data.List.union
    <$> emnlpCorrectionDictionary "data/emnlp_dict.txt"
    <*> utdallasCorrectionDictionary "data/utdallas.txt"

emnlpCorrectionDictionary :: FilePath -> IO CorrectionDictionary
emnlpCorrectionDictionary path = M.fromList . fmap (toPairs . T.words . TL.toStrict) . TL.lines . decodeUtf8 <$> BL.readFile path
    where toPairs (x:xs) = (CI.mk x, xs)

utdallasCorrectionDictionary :: FilePath -> IO CorrectionDictionary
utdallasCorrectionDictionary path = M.fromList . fmap (toPairs . T.words . TL.toStrict) . TL.lines . decodeUtf8 <$> BL.readFile path
    where toPairs (_:word:xs) = (CI.mk word, filter (/= "|") xs)
