{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Web.Obfuscate.THSpec
    where

import           Hashids
import           Test.Hspec
import           Web.Obfuscate
import           Web.Obfuscate.TH

-- Simple custom data type with one obfuscatable field
data TestFixture = TestFixture
    { _a :: ObIntegral Int
    , _b :: String
    }
    deriving (Show, Eq)

data NestedTestFixture = NestedTestFixture
    { _innerFixture :: TestFixture
    }
    deriving (Show, Eq)

-- These two lines replace all the need to write all of the instances
-- written by hand in Web.ObfuscateSpec
$(deriveObfuscate defaultObfuscationOptions ''TestFixture)
$(deriveObfuscate defaultObfuscationOptions ''NestedTestFixture)

-- Deriving standalone instances to make tests compile
deriving instance Show (ObfuscatedTestFixture)
deriving instance Eq (ObfuscatedTestFixture)

spec :: Spec
spec = do
    let (Right ctx) = mkHashidsContext "test-salt-please-ignore" 7 defaultAlphabet
    describe "Testing instances generated with Template Haskell" $ do
        it "can obfuscate and deobfuscate custom data types" $ do
            let testFixture = TestFixture
                                { _a = 1
                                , _b = "hello"
                                }

            deobfuscate ctx (obfuscate ctx testFixture)
              `shouldBe`
                Just testFixture

        it "ignores non obfuscateable values" $ do
            let testFixture = TestFixture
                                { _a = 1
                                , _b = "hello"
                                }
            let obfuscatedTestFixture = obfuscate ctx testFixture

            ob_b obfuscatedTestFixture
              `shouldBe`
                _b testFixture

        it "supports nested obfuscation" $ do
            let testFixture = TestFixture
                                { _a = 1
                                , _b = "hello"
                                }
            let nestedFixture = NestedTestFixture testFixture
            let obfuscatedTestFixture = obfuscate ctx testFixture
            let obfuscatedNestedTestFixture = obfuscate ctx nestedFixture

            ob_innerFixture obfuscatedNestedTestFixture
              `shouldBe`
                obfuscatedTestFixture
