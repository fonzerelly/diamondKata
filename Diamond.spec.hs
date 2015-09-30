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

   describe "createDiamond" $ do
      it "returns String \"A\" in case of char 'A'" $ do
         createDiamond 'A' `shouldBe` "A"

      -- it "returns \"A\nB B\nA\" in case of char 'B'" $ do
      --    createDiamond 'B' `shouldBe` "A\nB B\nA"
