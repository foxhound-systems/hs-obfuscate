{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Web.Obfuscate
    where

import           Control.Applicative  ( (<|>) )
import           Data.Aeson           ( Value (..) )
import           Data.Scientific      ( toBoundedInteger )
import           Database.Persist     ( Key, ToBackendKey )
import           Database.Persist.Sql ( SqlBackend, fromSqlKey, toSqlKey )
import           Text.Read            ( readMaybe )

import qualified Data.Maybe           as Maybe
import qualified Data.Text            as T
import qualified Hashids              as H

type family Obfuscated a
class CanObfuscate a where
  obfuscate :: H.HashidsContext -> a -> Obfuscated a
class CanDeobfuscate a where
  deobfuscate :: H.HashidsContext -> Obfuscated a -> Maybe a

type instance Obfuscated [a] = [Obfuscated a]
instance CanObfuscate a => CanObfuscate [a] where
  obfuscate ctx xs = fmap (obfuscate ctx) xs
instance CanDeobfuscate a => CanDeobfuscate [a] where
  deobfuscate ctx ys = traverse (deobfuscate ctx) ys

type instance Obfuscated (Maybe a) = Maybe (Obfuscated a)
instance CanObfuscate a => CanObfuscate (Maybe a) where
  obfuscate ctx xs = fmap (obfuscate ctx) xs
instance CanDeobfuscate a => CanDeobfuscate (Maybe a) where
  deobfuscate ctx ys = traverse (deobfuscate ctx) ys

obfuscateIntegral :: (Integral a) => H.HashidsContext -> a -> Value
obfuscateIntegral ctx i = String $ T.pack $ H.encode ctx [fromIntegral i]

deobfuscateIntegral :: (Read a, Bounded a, Integral a) => H.HashidsContext -> Value -> Maybe a
deobfuscateIntegral ctx (String r) = fromIntegral <$>
  (Maybe.listToMaybe $ H.decode ctx $ T.unpack r) <|> (readMaybe $ T.unpack r)

deobfuscateIntegral ctx (Number n) = toBoundedInteger n
deobfuscateIntegral ctx _ = Nothing

-- A general newtype wrapper for integrals that can be obfuscated
-- make a type alias for your particular use. Or use it as an example.
newtype ObIntegral a = ObIntegral a
  deriving newtype (Show, Read, Num, Eq, Ord, Enum, Real, Integral, Bounded)
type instance (Obfuscated (ObIntegral a)) = Value
instance Integral a => CanObfuscate (ObIntegral a) where
  obfuscate = obfuscateIntegral
instance (Read a, Bounded a, Integral a) => CanDeobfuscate (ObIntegral a) where
  deobfuscate = deobfuscateIntegral

type instance Obfuscated (Key e) = Value
instance (ToBackendKey SqlBackend e) => CanObfuscate (Key e) where
  obfuscate ctx i = obfuscateIntegral ctx $ fromSqlKey i
instance (ToBackendKey SqlBackend e) => CanDeobfuscate (Key e) where
  deobfuscate ctx r = toSqlKey <$> deobfuscateIntegral ctx r
