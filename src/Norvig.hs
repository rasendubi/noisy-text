module Norvig
    ( edits1
    , knownEdits1
    , knownEdits2
    ) where

import Control.Monad (liftM)
import Data.Char (isAlpha, isLower)
import Data.List (maximumBy)

import qualified Data.HashMap.Strict as HM

import qualified Data.Text as T

edits1 :: T.Text -> [T.Text]
edits1 word = concat
    [ removeLetter
    , swapLetter
    , replaceLetter
    , insertLetter
    ]
    where
        removeLetter = fmap dropChar [0 .. len-1]
        swapLetter = fmap swapChar [0 .. len-2]
        replaceLetter = concat $ fmap changeChar [0 .. len-1]
        insertLetter = concat $ fmap insertChars [0 .. len]

        len = T.length word
        alpha = '\'' : ['a' .. 'z']

        dropChar n = T.append s (T.tail e)
            where (s, e) = T.splitAt n word

        swapChar n = T.concat [ s, T.singleton (T.index e 1), T.singleton (T.index e 0), T.drop 2 e]
            where (s, e) = T.splitAt n word

        changeChar n = fmap (\c -> T.concat [s, T.singleton c, T.tail e]) alpha
            where (s, e) = T.splitAt n word

        insertChars n = fmap (\c -> T.concat [s, T.singleton c, e]) alpha
            where (s, e) = T.splitAt n word

knownEdits1 :: (T.Text -> Bool) -> T.Text -> [T.Text]
knownEdits1 inVocabulary x = filter inVocabulary $ edits1 x

knownEdits2 :: (T.Text -> Bool) -> T.Text -> [T.Text]
knownEdits2 inVocabulary x = filter inVocabulary $ edits1 x >>= edits1
