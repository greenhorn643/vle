{-# LANGUAGE DeriveGeneric #-}
module Data.VLE
  ( variableLengthEncode
  , variableLengthDecode
  , differenceEncode
  , differenceDecode
  , VLEWord64(..)
  ) where

import           Control.Applicative            ( Alternative((<|>), empty) )
import           Control.Monad
import           Data.Bits
import           Data.Foldable                  ( toList )
import           Data.List                      ( scanl1 )
import           Data.Serialize
import           Data.Word
import           GHC.Generics

newtype VLEWord64 = VLEWord64
  { unVLEWord64 :: Word64
  }
  deriving (Generic, Eq, Ord, Show)

instance Serialize VLEWord64 where
  put = variableLengthEncode . unVLEWord64
  get = VLEWord64 <$> variableLengthDecode

variableLengthEncode :: Putter Word64
variableLengthEncode x
  | x < 0x80 = putByte $ fromIntegral x
  | otherwise = do
    putByte $ fromIntegral $ (x .&. 0x7F) .|. 0x80
    variableLengthEncode $ x `shiftR` 7

variableLengthDecode :: Get Word64
variableLengthDecode = go 0 0
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

differenceEncode :: Foldable t => Putter (t Word64)
differenceEncode xs = do
  variableLengthEncode $ fromIntegral $ length xs
  go $ toList xs

 where
  go []       = return ()
  go (x : xs) = do
    variableLengthEncode x
    mapM_ variableLengthEncode (diffs x xs)

  diffs _ []       = []
  diffs x (y : ys) = y - x : diffs y ys

differenceDecode :: Alternative t => Get (t Word64)
differenceDecode = do
  length <- fromIntegral <$> variableLengthDecode
  ws     <- replicateM length variableLengthDecode
  let xs = scanl1 (+) ws
  return $ conv xs

 where
  x <| xs = pure x <|> xs

  conv = foldr (<|) empty
