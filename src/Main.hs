{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative ( (<$>), (<*>) )
import Control.Monad (forM_)

import System.IO.Unsafe (unsafePerformIO)
import System.Environment (getArgs)

import Data.Char (toLower)
import Data.List (union)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL (decodeUtf8, decodeLatin1)

import Data.Hashable (Hashable)
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM

import           Data.Aeson as JSON (ToJSON(toJSON))
import qualified Data.Aeson as JSON (decode', encode)
import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)

import           Data.CaseInsensitive ( CI )
import qualified Data.CaseInsensitive as CI

data Tweet = Tweet
    { tTid    :: {-# UNPACK #-}!T.Text
    , tIndex  :: {-# UNPACK #-}!T.Text
    , tInput  :: ![T.Text]
    , tSuggestions :: !(Maybe [HM.HashMap T.Text (HM.HashMap T.Text Double)])
    , tOutput :: !(Maybe [T.Text])
    } deriving (Eq, Show)

deriveJSON defaultOptions{fieldLabelModifier = fmap toLower . drop 1} ''Tweet

type Vocabulary = HS.HashSet (CI T.Text)
type CorrectionDictionary = HM.HashMap (CI T.Text) (HS.HashSet T.Text)

-- | WordSuggestion is function from word to hash map of suggested words
-- where value is a probability.
type WordSuggestion = T.Text -> HM.HashMap T.Text Double

suggestions :: HM.HashMap T.Text WordSuggestion
suggestions = HM.fromList
    [ ("id",   \x -> HM.singleton x 1)
    , ("oov",  oovSuggest)
    , ("corr", corrSuggest)
    ]

allSuggestions :: T.Text -> HM.HashMap T.Text (HM.HashMap T.Text Double)
allSuggestions x = fmap ($ x) suggestions

main :: IO ()
main = do
    args <- getArgs
    let fileName = case args of
                       [file] -> file
                       _      -> "data/test_data_20150430.json"

    Just tweets <- getTweets fileName
    let result = fmap (\t -> t{ tSuggestions = Just $ fmap allSuggestions $ tInput t}) tweets
    BL.putStrLn $ JSON.encode result

vocabulary :: Vocabulary
vocabulary = unsafePerformIO $ loadVocabulary "data/scowl.american.70"
{-# NOINLINE vocabulary #-}

oovSuggest :: WordSuggestion
oovSuggest x
    | HS.member (CI.mk x) vocabulary = HM.singleton x 1
    | otherwise                      = HM.empty


corrSuggest :: WordSuggestion
corrSuggest x = mapHashSet (const 1) $ HM.lookupDefault HS.empty (CI.mk x) correctionDictionary
    where
        correctionDictionary :: CorrectionDictionary
        correctionDictionary = unsafePerformIO $ HM.unionWith HS.union
            <$> emnlpCorrectionDictionary "data/emnlp_dict.txt"
            <*> utdallasCorrectionDictionary "data/utdallas.txt"
        {-# NOINLINE correctionDictionary #-}

getTweets :: FilePath -> IO (Maybe [Tweet])
getTweets path = JSON.decode' <$> BL.readFile path

loadVocabulary :: FilePath -> IO Vocabulary
loadVocabulary path = HS.fromList . fmap (CI.mk . TL.toStrict) . TL.lines . TL.decodeLatin1 <$> BL.readFile path

emnlpCorrectionDictionary :: FilePath -> IO CorrectionDictionary
emnlpCorrectionDictionary path = HM.fromList . fmap (toPairs . T.words . TL.toStrict) . TL.lines . TL.decodeUtf8 <$> BL.readFile path
    where toPairs (x:xs) = (CI.mk x, HS.fromList xs)

utdallasCorrectionDictionary :: FilePath -> IO CorrectionDictionary
utdallasCorrectionDictionary path = HM.fromList . fmap (toPairs . T.words . TL.toStrict) . TL.lines . TL.decodeUtf8 <$> BL.readFile path
    where toPairs (_:word:xs) = (CI.mk word, HS.fromList $ filter (/= "|") xs)

mapHashSet :: (Hashable a, Eq a) => (a -> b) -> HS.HashSet a -> HM.HashMap a b
mapHashSet f = HM.fromList . fmap (\x -> (x, f x)) . HS.toList
