import           Sample
import           Test.Hspec

main :: IO ()
main = hspec $ do
    describe "sample_function" $ do
        it "should return 785" $ do
            c_randomNumber `shouldReturn` 785
