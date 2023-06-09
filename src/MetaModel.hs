{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
module MetaModel
    where

import           MyPrelude
import qualified Data.Map as Map
import           GHC.Generics

class BasicSpdx3 a where
    summary :: a -> Text
    description :: a -> Text
    rawMetadata :: a -> Map.Map Text Text
    metadata :: a -> Text -> Maybe Text
    metadata a k = (Map.lookup k . rawMetadata) a
    name :: a -> Text
    default name :: Show a => a -> Text
    name a = case (`metadata` "name") a of
        Just n -> n
        _ -> trace (show a) undefined

data Spdx3Property
    = Spdx3Property
    { _propertySummary :: Text
    , _propertyDescription :: Text
    , _propertyMetadata :: Map.Map Text Text
    } deriving (Generic,Show)
instance FromJSON Spdx3Property where
    parseJSON = withObject "Spdx3Property" $ \v -> Spdx3Property
        <$> v .: "Summary"
        <*> v .: "Description"
        <*> v .:? "Metadata" .!= mempty
instance ToJSON Spdx3Property
instance BasicSpdx3 Spdx3Property where
    summary = _propertySummary
    description = _propertyDescription
    rawMetadata = _propertyMetadata

class PropertyContextSpdx3 a where
    getPropertyDetails :: Text -> a -> Maybe Spdx3Property
instance PropertyContextSpdx3 Spdx3Property where
    getPropertyDetails needle a = if needle == name a
                                  then Just a
                                  else Nothing

data Spdx3Vocabulary
    = Spdx3Vocabulary
    { _vocabularySummary :: Text
    , _vocabularyDescription :: Text
    , _vocabularyMetadata :: Map.Map Text Text
    , _vocabularyEntries :: Map.Map Text Text
    } deriving (Generic,Show)
instance FromJSON Spdx3Vocabulary where
    parseJSON = withObject "Spdx3Vocabulary" $ \v -> Spdx3Vocabulary
        <$> v .:? "Summary" .!= "MISSING" -- TODO
        <*> v .:? "Description" .!= "MISSING" -- TODO
        <*> v .:? "Metadata" .!= mempty
        <*> v .:? "Entries" .!= mempty
instance ToJSON Spdx3Vocabulary
instance BasicSpdx3 Spdx3Vocabulary where
    summary = _vocabularySummary
    description = _vocabularyDescription
    rawMetadata = _vocabularyMetadata

data Spdx3ClassPropertyParameters
    = Spdx3ClassPropertyParameters
    { _propertyType :: Text
    , _minCount :: Maybe Int
    , _maxCount :: Maybe Int
    } deriving (Generic,Show)
instance FromJSON Spdx3ClassPropertyParameters where
    parseJSON = withObject "Spdx3ClassPropertyParameters" $ \v -> Spdx3ClassPropertyParameters
        <$> v .: "type"
        <*> (fmap read <$> v .:? "minCount")
        <*> (fmap read <$> v .:? "maxCount")
instance ToJSON Spdx3ClassPropertyParameters

data Spdx3Class
    = Spdx3Class
    { _classSummary :: Text
    , _classDescription :: Text
    , _classMetadata :: Map.Map Text Text
    , _classProperties :: Map.Map Text Spdx3ClassPropertyParameters
    } deriving (Generic,Show)
instance FromJSON Spdx3Class where
    parseJSON = withObject "Spdx3Class" $ \v -> Spdx3Class
        <$> v .:? "Summary" .!= "MISSING" -- TODO
        <*> v .:? "Description" .!= "MISSING" -- TODO
        <*> v .:? "Metadata" .!= mempty
        <*> v .: "Properties"
instance ToJSON Spdx3Class
instance BasicSpdx3 Spdx3Class where
    summary = _classSummary
    description = _classDescription
    rawMetadata = _classMetadata

data Spdx3Profile
   = Spdx3Profile
   { _profileSummary :: Text
   , _profileDescription :: Text
   , _profileMetadata :: Map.Map Text Text
   , _profileProperties :: Map.Map Text Spdx3Property
   , _profileVocabularies :: Map.Map Text Spdx3Vocabulary
   , _profileClasses :: Map.Map Text Spdx3Class
   } deriving (Generic,Show)
instance FromJSON Spdx3Profile where
    parseJSON = withObject "Spdx3Profile" $ \v -> Spdx3Profile
        <$> v .:? "Summary" .!= "MISSING" -- TODO
        <*> v .:? "Description" .!= "MISSING" -- TODO
        <*> v .:? "Metadata" .!= mempty
        <*> v .: "Properties"
        <*> v .: "Vocabularies"
        <*> v .: "Classes"
instance ToJSON Spdx3Profile
instance BasicSpdx3 Spdx3Profile where
    summary = _profileSummary
    description = _profileDescription
    rawMetadata = _profileMetadata
instance PropertyContextSpdx3 Spdx3Profile where
    getPropertyDetails needle (Spdx3Profile {_profileProperties = ps}) = needle `Map.lookup` ps

newtype Spdx3Model
    = Spdx3Model
    { _profiles :: Map.Map Text Spdx3Profile
    } deriving (Generic,Show)
instance FromJSON Spdx3Model where
    parseJSON = fmap Spdx3Model . parseJSON
instance ToJSON Spdx3Model
instance PropertyContextSpdx3 Spdx3Model where
    getPropertyDetails needle = msum . map (getPropertyDetails needle . snd) . Map.assocs . _profiles