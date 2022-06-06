{-# LANGUAGE DeriveGeneric #-}
module Data.VLE
    ( variableLengthEncode
    , variableLengthDecode
    , VLEWord64(..)
    ) where

import           Data.Bits
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
  | x < 0x80  = putByte $ fromIntegral x
  | otherwise = do  putByte $ fromIntegral $ (x .&. 0x7F) .|. 0x80
                    variableLengthEncode $ x `shiftR` 7

variableLengthDecode :: Get Word64
variableLengthDecode = go 0 0
  where go 70 _ = fail "VLE overflow"
        go 63 w = do  x <- getByte
                      case x of
                        0 -> return w
                        1 -> return (w .|. (1 `shiftL` 63))
                        _ -> fail "VLE overflow"
        go b w  = do  x <- fromIntegral <$> getByte
                      let w' = w .|. ((x .&. 0x7F) `shiftL` b)
                      if x .&. 0x80 == 0
                        then return w'
                      else go (b + 7) w'

putByte :: Putter Word8
putByte = put

getByte :: Get Word8
getByte = get
