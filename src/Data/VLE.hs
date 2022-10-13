{-# LANGUAGE DeriveGeneric #-}

module Data.VLE
  ( variableLengthEncode,
    variableLengthDecode,
    differenceEncode,
    differenceDecode,
    VLEInt64 (..),
  )
where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad
import Data.Bits
import Data.Foldable (toList)
import Data.Int
import Data.List (scanl1)
import Data.Serialize
import Data.Word (Word64, Word8)
import GHC.Generics

newtype VLEInt64 = VLEInt64
  { unVLEInt64 :: Int64
  }
  deriving (Generic, Eq, Ord, Show)

instance Serialize VLEInt64 where
  put = variableLengthEncode . unVLEInt64
  get = VLEInt64 <$> variableLengthDecode

variableLengthEncode :: Putter Int64
variableLengthEncode = variableLengthEncodeW64 . fromIntegral
  where
    variableLengthEncodeW64 :: Putter Word64
    variableLengthEncodeW64 x
      | x < 0x80 = putByte $ fromIntegral x
      | otherwise = do
        putByte $ fromIntegral $ (x .&. 0x7F) .|. 0x80
        variableLengthEncodeW64 $ x `shiftR` 7

variableLengthDecode :: Get Int64
variableLengthDecode = fromIntegral <$> variableLengthDecodeW64
  where
    variableLengthDecodeW64 :: Get Word64
    variableLengthDecodeW64 = go 0 0
      where
        go 70 _ = fail "VLE overflow"
        go 63 w = do
          x <- getByte
          case x of
            0 -> return w
            1 -> return (w .|. (1 `shiftL` 63))
            _ -> fail "VLE overflow"
        go b w = do
          x <- fromIntegral <$> getByte
          let w' = w .|. ((x .&. 0x7F) `shiftL` b)
          if x .&. 0x80 == 0 then return w' else go (b + 7) w'

putByte :: Putter Word8
putByte = put

getByte :: Get Word8
getByte = get

differenceEncode :: Foldable t => Putter (t Int64)
differenceEncode xs = do
  variableLengthEncode $ fromIntegral $ length xs
  go $ toList xs
  where
    go [] = return ()
    go (x : xs) = do
      variableLengthEncode x
      mapM_ variableLengthEncode (diffs x xs)

    diffs _ [] = []
    diffs x (y : ys) = y - x : diffs y ys

differenceDecode :: Alternative t => Get (t Int64)
differenceDecode = do
  length <- fromIntegral <$> variableLengthDecode
  ws <- replicateM length variableLengthDecode
  let xs = scanl1 (+) ws
  return $ conv xs
  where
    x <| xs = pure x <|> xs

    conv = foldr (<|) empty
