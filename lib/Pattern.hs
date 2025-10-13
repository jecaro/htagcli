module Pattern
  ( Formatting (..),
    Fragment (..),
    Slashes (..),
    slashesAsText,
    parseSlashes,
    Spaces (..),
    spacesAsText,
    parseSpaces,
    Pattern,
    asText,
    parser,
    noFormatting,
    format,
    tags,
  )
where

import AudioTrack qualified
import Control.Applicative.Combinators.NonEmpty qualified as NonEmpty
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Sound.HTagLib qualified as HTagLib
import Sound.HTagLib.Extra qualified as HTagLib
import Tag qualified
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char qualified as Megaparsec
import Text.Printf qualified as Text

data Fragment = Text Text | Placeholder Tag.Tag
  deriving (Eq, Show)

type Component = NonEmpty Fragment

type Pattern = NonEmpty Component

type Parser = Megaparsec.Parsec Void Text

data Slashes = SlRemove | SlToUnderscore
  deriving (Show, Eq, Enum, Bounded)

data Spaces = SpKeep | SpToUnderscore
  deriving (Show, Eq, Enum, Bounded)

data Formatting = Formatting
  { foSlashes :: Slashes,
    foSpaces :: Spaces,
    foPadTrackNumbers :: Int
  }
  deriving (Show, Eq)

noFormatting :: Formatting
noFormatting =
  Formatting
    { foSlashes = SlRemove,
      foSpaces = SpKeep,
      foPadTrackNumbers = 0
    }

parser :: Parser Pattern
parser =
  (componentParser `NonEmpty.sepBy1` Megaparsec.char '/') <* Megaparsec.eof

tags :: Pattern -> [Tag.Tag]
tags = foldMap $ foldMap tagList
  where
    tagList (Placeholder tag) = [tag]
    tagList (Text _) = []

componentParser :: Parser Component
componentParser = NonEmpty.some fragmentParser

fragmentParser :: Parser Fragment
fragmentParser = Placeholder <$> placeholderParser <|> Text <$> textParser

textParser :: Parser Text
textParser = toText <$> Megaparsec.some (Megaparsec.satisfy notSlashNorBrace)
  where
    notSlashNorBrace c = c /= '/' && c /= '{' && c /= '}'

placeholderParser :: Parser Tag.Tag
placeholderParser =
  Megaparsec.between (Megaparsec.char '{') (Megaparsec.char '}') Tag.parser

format :: Formatting -> AudioTrack.AudioTrack -> Pattern -> Text
format formatting = formatWith . formatTag formatting

asText :: Pattern -> Text
asText = formatWith Tag.asText

slashesAsText :: Slashes -> Text
slashesAsText SlRemove = "remove"
slashesAsText SlToUnderscore = "to_underscore"

parseSlashes :: Text -> Either Text Slashes
parseSlashes "remove" = Right SlRemove
parseSlashes "to_underscore" = Right SlToUnderscore
parseSlashes _ = Left "Should be one of 'remove' or 'to_underscore'"

spacesAsText :: Spaces -> Text
spacesAsText SpKeep = "keep"
spacesAsText SpToUnderscore = "to_underscore"

parseSpaces :: Text -> Either Text Spaces
parseSpaces "keep" = Right SpKeep
parseSpaces "to_underscore" = Right SpToUnderscore
parseSpaces _ = Left "Should be one of 'keep' or 'to_underscore'"

formatWith :: (Tag.Tag -> Text) -> Pattern -> Text
formatWith formatter pattern =
  fold $ NonEmpty.intersperse "/" $ formatComponentWith formatter <$> pattern

formatComponentWith :: (Tag.Tag -> Text) -> Component -> Text
formatComponentWith formatter = foldMap (formatFragmentWith formatter)

formatFragmentWith :: (Tag.Tag -> Text) -> Fragment -> Text
formatFragmentWith _ (Text text) = text
formatFragmentWith formatter (Placeholder tag) = formatter tag

formatTag :: Formatting -> AudioTrack.AudioTrack -> Tag.Tag -> Text
formatTag formatting AudioTrack.AudioTrack {..} Tag.Title =
  textFormatter formatting . HTagLib.unTitle $ atTitle
formatTag formatting AudioTrack.AudioTrack {..} Tag.Artist =
  textFormatter formatting . HTagLib.unArtist $ atArtist
formatTag formatting AudioTrack.AudioTrack {..} Tag.Album =
  textFormatter formatting . HTagLib.unAlbum $ atAlbum
formatTag formatting AudioTrack.AudioTrack {..} Tag.AlbumArtist =
  textFormatter formatting . HTagLib.unAlbumArtist $ atAlbumArtist
formatTag formatting AudioTrack.AudioTrack {..} Tag.Genre =
  textFormatter formatting . HTagLib.unGenre $ atGenre
formatTag _ AudioTrack.AudioTrack {..} Tag.Year =
  maybe "" (show . HTagLib.unYear) atYear
formatTag formatting AudioTrack.AudioTrack {..} Tag.Track =
  maybe "" (trackNumberFormat formatting . HTagLib.unTrackNumber) atTrack

textFormatter :: Formatting -> Text -> Text
textFormatter Formatting {..} = spaces foSpaces . slashes foSlashes
  where
    spaces SpKeep = id
    spaces SpToUnderscore = Text.map spaceToUnderscore
    slashes SlRemove = Text.filter (/= '/')
    slashes SlToUnderscore = Text.map slashToUnderscore
    slashToUnderscore '/' = '_'
    slashToUnderscore c = c
    spaceToUnderscore ' ' = '_'
    spaceToUnderscore c = c

trackNumberFormat :: Formatting -> Int -> Text
trackNumberFormat Formatting {..} =
  Text.pack . Text.printf ("%0" <> show foPadTrackNumbers <> "d")
