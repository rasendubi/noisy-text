{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
import Control.Applicative ( (<$>), (<*>) )
import Control.Monad (guard)

import System.IO.Unsafe (unsafePerformIO)
import System.Environment (getArgs)

import Data.Char (toLower, isPunctuation, isNumber, isSymbol)
import Data.List (foldl')

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

import Norvig

type Suggestions = HM.HashMap T.Text Double

data Tweet = Tweet
    { tTid    :: {-# UNPACK #-}!T.Text
    , tIndex  :: {-# UNPACK #-}!T.Text
    , tInput  :: ![T.Text]
    , tSuggestions :: !(Maybe [HM.HashMap T.Text Suggestions])
    , tOutput :: !(Maybe [T.Text])
    } deriving (Eq, Show)

deriveJSON defaultOptions{fieldLabelModifier = fmap toLower . drop 1} ''Tweet

type Vocabulary = HS.HashSet (CI T.Text)
type CorrectionDictionary = HM.HashMap (CI T.Text) (HS.HashSet T.Text)

suggestions :: HM.HashMap T.Text (T.Text -> Suggestions)
suggestions = HM.fromList
    [ ("id", flagSuggestion (const True))
    , ("vocabulary", vocabularySuggest)
    , ("corr", corrSuggest)
    , ("exclude", excludeSuggest)
    , ("split", splitSuggest)
    , ("edits1", edits1Suggest)
    --, ("edits2", edits2Suggest)
    ]

allSuggestions :: T.Text -> HM.HashMap T.Text Suggestions
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

inVocabulary :: T.Text -> Bool
inVocabulary x = HS.member (CI.mk x) vocabulary

vocabularySuggest :: T.Text -> Suggestions
vocabularySuggest = flagSuggestion inVocabulary

corrSuggest :: T.Text -> Suggestions
corrSuggest x = mapHashSet (const 1) $ HM.lookupDefault HS.empty (CI.mk x) correctionDictionary
    where
        correctionDictionary :: CorrectionDictionary
        correctionDictionary = unsafePerformIO $ HM.unionWith HS.union
            <$> emnlpCorrectionDictionary "data/emnlp_dict.txt"
            <*> utdallasCorrectionDictionary "data/utdallas.txt"
        {-# NOINLINE correctionDictionary #-}

excludeSuggest :: T.Text -> Suggestions
excludeSuggest = flagSuggestion $ \x -> T.head x == '#' || T.head x == '@' || T.all (\c -> isSymbol c || isPunctuation c || isNumber c) x

splitSuggest :: T.Text -> Suggestions
splitSuggest x = HM.delete x $ splitSuggest' x
    where
        splitSuggest' word
            | inVocabulary word = HM.singleton word 1
            | otherwise = foldl' suggestionUnion HM.empty $ do
                (x, y) <- allSplits
                guard $ inVocabulary x
                let next = splitSuggest' y
                return $ mapKeyValue (\(k,v) -> (T.concat [x, " ", k], succ v)) next
            where allSplits = tail $ init $ zip (T.inits word) (T.tails word)

edits1Suggest :: T.Text -> Suggestions
edits1Suggest = HM.fromList . fmap (,1) . knownEdits1 inVocabulary

edits2Suggest :: T.Text -> Suggestions
edits2Suggest = HM.fromList . fmap (,1) . knownEdits1 (\x -> HS.member (CI.mk x) extendedVocabulary)
    where extendedVocabulary = HS.fromList . fmap CI.mk . concatMap (edits1 . CI.original) $ HS.toList vocabulary

flagSuggestion :: (T.Text -> Bool) -> T.Text -> Suggestions
flagSuggestion p x = if p x then HM.singleton x 1 else HM.empty

getTweets :: FilePath -> IO (Maybe [Tweet])
getTweets path = JSON.decode' <$> BL.readFile path

loadVocabulary :: FilePath -> IO Vocabulary
loadVocabulary path = HS.fromList . fmap (CI.mk . TL.toStrict) . TL.lines . TL.decodeLatin1 <$> BL.readFile path

emnlpCorrectionDictionary :: FilePath -> IO CorrectionDictionary
emnlpCorrectionDictionary = parseDictionary toPairs
    where toPairs (x:xs) = (CI.mk x, HS.fromList xs)

utdallasCorrectionDictionary :: FilePath -> IO CorrectionDictionary
utdallasCorrectionDictionary = parseDictionary toPairs
    where toPairs (_:word:xs) = (CI.mk word, HS.fromList $ filter (/= "|") xs)

parseDictionary :: (Hashable k, Eq k) => ([T.Text] -> (k, v)) -> FilePath -> IO (HM.HashMap k v)
parseDictionary toPairs path = HM.fromList . fmap (toPairs . T.words . TL.toStrict) . TL.lines . TL.decodeUtf8 <$> BL.readFile path

mapHashSet :: (Hashable a, Eq a) => (a -> b) -> HS.HashSet a -> HM.HashMap a b
mapHashSet f = HM.fromList . fmap (\x -> (x, f x)) . HS.toList

mapKeyValue :: (Eq c, Hashable c) => ((a,b) -> (c,d)) -> HM.HashMap a b -> HM.HashMap c d
mapKeyValue f = HM.fromList . fmap f . HM.toList

suggestionUnion :: Suggestions -> Suggestions -> Suggestions
suggestionUnion = HM.unionWith (+)
