{-# LANGUAGE ForeignFunctionInterface #-}
module KenLM where

import Control.Applicative ( (<$>) )

import Data.Coerce (coerce)

import qualified Data.ByteString as BS

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Array

import System.IO.Unsafe (unsafePerformIO)

import Control.DeepSeq

foreign import ccall unsafe "noisy_text_kenlm.h language_model"
    c_language_model
        :: Ptr CString
        -> CInt
        -> Ptr CString
        -> CInt
        -> Ptr CString
        -> CInt
        -> Ptr CDouble
        -> IO ()

useAsCStringList :: [BS.ByteString] -> ([CString] -> IO a) -> IO a
useAsCStringList [] cb     = cb []
useAsCStringList (x:xs) cb = BS.useAsCString x $ \x' -> useAsCStringList xs (\xs' -> cb (x':xs'))

languageModel :: [BS.ByteString] -> [BS.ByteString] -> [BS.ByteString] -> [Double]
languageModel part1 part2 cands =
    unsafePerformIO $
    useAsCStringList part1 $ \pp1 -> withArrayLen pp1 $ \part1_len p_part1 ->
    useAsCStringList part2 $ \pp2 -> withArrayLen pp2 $ \part2_len p_part2 ->
    useAsCStringList cands $ \pca -> withArrayLen pca $ \cands_len p_cands ->
    allocaArray cands_len $ \results -> do
        c_language_model
            p_part1 (fromIntegral part1_len)
            p_part2 (fromIntegral part2_len)
            p_cands (fromIntegral cands_len)
            results
        coerce <$> peekArray cands_len results

