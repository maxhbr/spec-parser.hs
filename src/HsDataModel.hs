module HsDataModel
    where

import MyPrelude
import qualified Data.Map as Map

class BasicSpdx3 a where
    summary :: a -> Text
    description :: a -> Text
    metadata :: a -> Text -> Maybe Text

data Spdx3Property
    = Spdx3Property
    { _propertySummary :: Text
    , _PropertyDescription :: Text
    , _propertyMetadata :: Map.Map Text Text
    } deriving (Show)
data Spdx3Vocabulary
    = Spdx3Vocabulary
    { _vocabularySummary :: Text
    , _vocabularyDescription :: Text
    , _vocabularyMetadata :: Map.Map Text Text
    } deriving (Show)
data Spdx3Class
    = Spdx3Class
    { _classSummary :: Text
    , _classDescription :: Text
    , _classMetadata :: Map.Map Text Text
    } deriving (Show)
data Spdx3Profile
   = Spdx3Profile
   { _profileSummary :: Text
   , _profileDescription :: Text
   , _profileMetadata :: Map.Map Text Text
   , _profileProperties :: [Spdx3Property]
   , _profileVocabularies :: [Spdx3Vocabulary]
   , _profileClasses :: [Spdx3Class]
   } deriving (Show)
instance FromJSON Spdx3Profile where
    parseJSON = withObject "Spdx3Profile" $ \v -> Spdx3Profile
        <$> v .: "Summary"
        <*> v .: "Description"
        <*> v .: "meta"
        <*> pure []
        <*> pure []
        <*> pure []


newtype Spdx3Model
    = Spdx3Model
    { _profiles :: Map.Map Text Spdx3Profile
    } deriving (Show)
instance FromJSON Spdx3Model where
    parseJSON = fmap Spdx3Model . parseJSON