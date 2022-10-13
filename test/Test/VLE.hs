module Test.VLE
  ( specs
  ) where

import           Test.Hspec
import qualified Data.ByteString                                       as B
import qualified Data.ByteString.Lazy                                  as LB
import           Data.Serialize
import           Data.VLE

spec_EncodesCorrectLengths :: Spec
spec_EncodesCorrectLengths =
  describe "Encodes correct lengths" $ do
    it "encodes 0 in 1 byte" $
      B.length (runPut $ variableLengthEncode 0) `shouldBe` 1
    it "encodes 1 in 1 byte" $
      B.length (runPut $ variableLengthEncode 1) `shouldBe` 1
    it "encodes 127 in 1 byte" $
      B.length (runPut $ variableLengthEncode 127) `shouldBe` 1
    it "encodes 128 in 2 bytes" $
      B.length (runPut $ variableLengthEncode 128) `shouldBe` 2
    it "encodes 16383 in 2 bytes" $
      B.length (runPut $ variableLengthEncode 16383) `shouldBe` 2
    it "encodes 16384 in 3 bytes" $
      B.length (runPut $ variableLengthEncode 16384) `shouldBe` 3
    it "encodes 2097151 in 3 bytes" $
      B.length (runPut $ variableLengthEncode 2097151) `shouldBe` 3
    it "encodes 2097152 in 4 bytes" $
      B.length (runPut $ variableLengthEncode 2097152) `shouldBe` 4
    it "encodes 2097152 in 4 bytes" $
      B.length (runPut $ variableLengthEncode 2097152) `shouldBe` 4
    it "encodes 9223372036854775807 in 9 bytes" $
      B.length (runPut $ variableLengthEncode 9223372036854775807) `shouldBe` 9
    it "encodes 9223372036854775808 in 1 byte" $
      B.length (runPut $ variableLengthEncode 9223372036854775808) `shouldBe` 10
    it "encodes 18446744073709551615 in 1 bytes" $
      B.length (runPut $ variableLengthEncode 18446744073709551615) `shouldBe` 10

spec_DecodeIsEncodeInverse :: Spec
spec_DecodeIsEncodeInverse =
  describe "Decode inverts encoding" $ do
    it "decode $ encode 0 == 0" $
      runGet variableLengthDecode (runPut $ variableLengthEncode 0) `shouldBe` Right 0
    it "decode $ encode 1 == 1" $
      runGet variableLengthDecode (runPut $ variableLengthEncode 1) `shouldBe` Right 1
    it "decode $ encode x == x for arbitrary x (1 of 6)" $
      runGet variableLengthDecode (runPut $ variableLengthEncode 3912837192) `shouldBe` Right 3912837192
    it "decode $ encode x == x for arbitrary x (2 of 6)" $
      runGet variableLengthDecode (runPut $ variableLengthEncode 6636238866704895954) `shouldBe` Right 6636238866704895954
    it "decode $ encode x == x for arbitrary x (3 of 6)" $
      runGet variableLengthDecode (runPut $ variableLengthEncode 5280046270) `shouldBe` Right 5280046270
    it "decode $ encode x == x for arbitrary x (4 of 6)" $
      runGet variableLengthDecode (runPut $ variableLengthEncode 8259000208452) `shouldBe` Right 8259000208452
    it "decode $ encode x == x for arbitrary x (5 of 6)" $
      runGet variableLengthDecode (runPut $ variableLengthEncode 2824375) `shouldBe` Right 2824375
    it "decode $ encode x == x for arbitrary x (6 of 6)" $
      fromIntegral <$> runGet variableLengthDecode (runPut $ variableLengthEncode (-1)) `shouldBe` Right ((-1) :: Integer)

spec_EncodeIsDecodePseudoInverse :: Spec
spec_EncodeIsDecodePseudoInverse =
  describe "Encode inverts decoding when decoding is valid" $ do
    it "encode $ decode [0x00] == [0x00]" $
      runPut . variableLengthEncode <$>
      runGet variableLengthDecode (B.pack [0x00]) `shouldBe`
      Right (B.pack [0x00])
    it "encode $ decode [0x01] == [0x01]" $
      runPut . variableLengthEncode <$>
      runGet variableLengthDecode (B.pack [0x01]) `shouldBe`
      Right (B.pack [0x01])
    it "encode $ decode [x1,x2..] == [x1,x2..] for arbitrary valid [x1,x2..] (1 of 5)" $
      runPut . variableLengthEncode <$>
      runGet variableLengthDecode (B.pack [0xF2, 0xB9, 0xDe, 0xCE, 0x7E]) `shouldBe`
      Right (B.pack [0xF2, 0xB9, 0xDe, 0xCE, 0x7E])
    it "encode $ decode [x1,x2..] == [x1,x2..] for arbitrary valid [x1,x2..] (2 of 5)" $
      runPut . variableLengthEncode <$>
      runGet variableLengthDecode (B.pack [0xB7, 0x85, 0xC2, 0x38]) `shouldBe`
      Right (B.pack [0xB7, 0x85, 0xC2, 0x38])
    it "encode $ decode [x1,x2..] == [x1,x2..] for arbitrary valid [x1,x2..] (3 of 5)" $
      runPut . variableLengthEncode <$>
      runGet
        variableLengthDecode
        (B.pack [0xC2, 0x93, 0xBF, 0x98, 0xD3, 0xB1, 0xAD, 0xDB, 0x45]) `shouldBe`
      Right (B.pack [0xC2, 0x93, 0xBF, 0x98, 0xD3, 0xB1, 0xAD, 0xDB, 0x45])
    it "encode $ decode [x1,x2..] == [x1,x2..] for arbitrary valid [x1,x2..] (4 of 5)" $
      runPut . variableLengthEncode <$>
      runGet variableLengthDecode (B.pack [0x95, 0xB2, 0x11]) `shouldBe`
      Right (B.pack [0x95, 0xB2, 0x11])
    it "encode $ decode [x1,x2..] == [x1,x2..] for arbitrary valid [x1,x2..] (5 of 5)" $
      runPut . variableLengthEncode <$>
      runGet variableLengthDecode (B.pack [0xBE, 0xFB, 0x85, 0x6B]) `shouldBe`
      Right (B.pack [0xBE, 0xFB, 0x85, 0x6B])

spec_DecodeFailsOnOverflow :: Spec
spec_DecodeFailsOnOverflow =
  describe "Decoding fails with error \"VLE overflow\" if the encoded number is too large for a 64 bit unsigned integer" $ do
    it "decode [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF] fails" $
      runGet
        variableLengthDecode
        (B.pack [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]) `shouldBe`
      Left "Failed reading: VLE overflow\nEmpty call stack\n"
    it "lazy decoding an infinite list fails without bottoming out" $
      runGetLazy variableLengthDecode (LB.pack $ repeat 0xFF) `shouldBe`
      Left "Failed reading: VLE overflow\nEmpty call stack\n"
    it "decode [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x01] succeeds" $
      runGet
        variableLengthDecode
        (B.pack [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x01]) `shouldBe`
      Right 18446744073709551615
    it "decode [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x02] fails" $
      runGet
        variableLengthDecode
        (B.pack [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x02]) `shouldBe`
      Left "Failed reading: VLE overflow\nEmpty call stack\n"

spec_DecodeFailsOnNotEnoughInput :: Spec
spec_DecodeFailsOnNotEnoughInput =
  describe "Decoding fails with error \"Not enough input\" if the encoded number is not terminated" $ do
    it "decode [0x83, 0xA3, 0xF2, 0x89] fails" $
      runGet variableLengthDecode (B.pack [0x83, 0xA3, 0xF2, 0x89]) `shouldBe`
      Left "too few bytes\nFrom:\tdemandInput\n\n"
    it "decode [0xFF] fails" $
      runGet variableLengthDecode (B.pack [0xFF]) `shouldBe`
      Left "too few bytes\nFrom:\tdemandInput\n\n"

spec_DifferenceDecodeIsEncodeInverse :: Spec
spec_DifferenceDecodeIsEncodeInverse =
  describe "Difference decode inverts difference encoding" $ do
    it "differenceDecode $ differenceEncode [] = []" $
      runGet differenceDecode (runPut $ differenceEncode []) `shouldBe`
      Right []
    it "differenceDecode $ differenceEncode [42] = [42]" $
      runGet differenceDecode (runPut $ differenceEncode [42]) `shouldBe`
      Right [42]
    it "differenceDecode $ differenceEncode [1,2,3,4] = [1,2,3,4]" $
      runGet differenceDecode (runPut $ differenceEncode [1,2,3,4]) `shouldBe`
      Right [1,2,3,4]

specs :: Spec
specs =
  describe "VLE" $ do
    spec_EncodesCorrectLengths
    spec_DecodeIsEncodeInverse
    spec_EncodeIsDecodePseudoInverse
    spec_DecodeFailsOnOverflow
    spec_DecodeFailsOnNotEnoughInput
    spec_DifferenceDecodeIsEncodeInverse
