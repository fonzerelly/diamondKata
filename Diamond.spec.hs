import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Diamond

main :: IO ()
main = hspec $ do
   describe "inner" $ do
      it "returns empty String on 'A'" $ do
         inner 'A' `shouldBe` ""

      it "returns one Whitespace on 'B'" $ do
         inner 'B' `shouldBe` " "

      it "returns three Whitespaces on 'C'" $ do
         inner 'C' `shouldBe` "   "

      it "returns five whitespaces on 'D'" $ do
         inner 'D' `shouldBe` "     "

      it "returns seven Whitespaces on 'E'" $ do
         inner 'E' `shouldBe` "       "

   describe "outer" $ do
      it "return empty string max size char" $ do
         outer 9 'E' `shouldBe` ""

      it "return one whitespace on max size predecessor" $ do
         outer 9 'D' `shouldBe` " "

      it "return half of max size for 'A'" $ do
         outer 9 'A' `shouldBe` "    "

   describe "defineDiamondLines" $ do
      it "returns A on 'A'" $ do
         defineDiamondLines 'A' `shouldBe` "A"

      it "returns ABA on 'B'" $ do
         defineDiamondLines 'B' `shouldBe` "ABA"

      it "returns ABCBA on 'C'" $ do
         defineDiamondLines 'C' `shouldBe` "ABCBA"

   describe "createDiamondLine" $ do
      it "returns Empty String when maxLength is zero" $ do
         createDiamondLine 0 'Z' `shouldBe` ""

      it "returns A on single A" $ do
         createDiamondLine 1 'A' `shouldBe` "A"

      it "returns maxlen on A" $ do
         createDiamondLine 5 'A' `shouldBe` "  A"

      it "returns maxlen on B" $ do
         createDiamondLine 5 'B' `shouldBe` " B B"

      it "returns maxlen on C" $ do
         createDiamondLine 5 'C' `shouldBe` "C   C"


   describe "createDiamond" $ do
      it "returns String \"A\" in case of char 'A'" $ do
         createDiamond 'A' `shouldBe` "A"

      it "returns \"A\nB B\nA\" in case of char 'B'" $ do
         createDiamond 'B' `shouldBe` " A\nB B\n A\n"

      it "returns diamond of C" $ do
         createDiamond 'C' `shouldBe` "  A\n B B\nC   C\n B B\n  A\n"

      -- it "returns \"A\nB B\nA\" in case of char 'B'" $ do
      --    createDiamond 'B' `shouldBe` " A\nB B\n A\n"
