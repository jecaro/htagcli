module Config (Config (..), defaultConfig) where

import Data.Aeson (
    FromJSON,
    Options (..),
    ToJSON (..),
    camelTo2,
    defaultOptions,
    genericParseJSON,
    genericToJSON,
    parseJSON,
 )

newtype Config = Config
    { coGenres :: [Text]
    }
    deriving (Eq, Generic, Show)

customOptions :: Options
customOptions = defaultOptions{fieldLabelModifier = camelTo2 '-' . drop 2}

instance ToJSON Config where
    toJSON = genericToJSON customOptions

instance FromJSON Config where
    parseJSON = genericParseJSON customOptions

defaultConfig :: Config
defaultConfig =
    Config
        { coGenres =
            [ "Blues"
            , "Chanson Française"
            , "Classical"
            , "Country"
            , "Divers"
            , "Electronica"
            , "Hardcore"
            , "Hip-Hop"
            , "International"
            , "Jazz"
            , "Latin"
            , "Original Soundtrack"
            , "Pop/Rock"
            , "Pour les enfants"
            , "Punk"
            ]
        }
