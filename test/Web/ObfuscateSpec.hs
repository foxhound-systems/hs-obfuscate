{-# LANGUAGE TypeFamilies #-}
module Web.ObfuscateSpec
    where

import           Data.Either          ( fromRight )
import           Data.Int             ( Int64 )
import           Database.Persist.Sql ( Key, toSqlKey )
import           Hashids
import           Test.Hspec
import           Web.Obfuscate

data TestFixture = TestFixture
    { _a :: ObIntegral Int
    , _b :: String
    }
    deriving (Show, Eq)

data ObfuscatedTestFixture = ObfuscatedTestFixture
    { ob_a :: Obfuscated (ObIntegral Int)
    , ob_b :: String
    }
    deriving (Show, Eq)

type instance Obfuscated TestFixture = ObfuscatedTestFixture
instance CanObfuscate TestFixture where
    obfuscate ctx fixture =
        ObfuscatedTestFixture
            { ob_a = obfuscate ctx (_a fixture)
            , ob_b = _b fixture
            }
instance CanDeobfuscate TestFixture where
    deobfuscate ctx fixture = do
        a <- deobfuscate ctx (ob_a fixture)
        pure $ TestFixture
            { _a = a
            , _b = ob_b fixture
            }

data NestedTestFixture = NestedTestFixture
    { _innerFixture :: TestFixture
    }
    deriving (Show, Eq)

data ObfuscatedNestedTestFixture = ObfuscatedNestedTestFixture
    { ob_innerFixture :: Obfuscated TestFixture
    }

type instance Obfuscated NestedTestFixture = ObfuscatedNestedTestFixture
instance CanObfuscate NestedTestFixture where
    obfuscate ctx fixture =
        ObfuscatedNestedTestFixture
            { ob_innerFixture = obfuscate ctx (_innerFixture fixture)
            }
instance CanDeobfuscate NestedTestFixture where
    deobfuscate ctx fixture = do
        innerFixture <- deobfuscate ctx (ob_innerFixture fixture)
        pure $ NestedTestFixture
            { _innerFixture = innerFixture
            }

spec :: Spec
spec = do
    let (Right hashidsContext) = mkHashidsContext "mySaltGoesHere" 7 defaultAlphabet
    describe "Defaults " $ do
        it "can obfuscate and deobfuscate integers" $ do
            let i = 12312 :: Int
            deobfuscate hashidsContext (obfuscate hashidsContext (ObIntegral i)) `shouldBe` Just (ObIntegral i)

        it "can obfuscate and deobfuscate custom data types" $ do
            let testFixture = TestFixture
                                { _a = 1
                                , _b = "hello"
                                }
            deobfuscate hashidsContext (obfuscate hashidsContext testFixture) `shouldBe` Just testFixture

        it "ignores non obfuscateable values" $ do
            let testFixture = TestFixture
                                { _a = 1
                                , _b = "hello"
                                }
            let obfuscatedTestFixture = obfuscate hashidsContext testFixture
            ob_b obfuscatedTestFixture `shouldBe` _b testFixture

        it "supports nested obfuscation" $ do
            let testFixture = TestFixture
                                { _a = 1
                                , _b = "hello"
                                }
            let nestedFixture = NestedTestFixture testFixture
            let obfuscatedTestFixture = obfuscate hashidsContext testFixture
            let obfuscatedNestedTestFixture = obfuscate hashidsContext nestedFixture
            ob_innerFixture obfuscatedNestedTestFixture `shouldBe` obfuscatedTestFixture

