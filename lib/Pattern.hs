module Pattern
  ( Pattern,
    Fragment (..),
    format,
    parser,
    asText,
  )
where

import AudioTrack qualified
import Control.Applicative.Combinators.NonEmpty qualified as NonEmpty
import Data.List.NonEmpty qualified as NonEmpty
import Tag qualified
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char qualified as Megaparsec

data Fragment = Text Text | Placeholder Tag.Tag
  deriving (Eq, Show)

type Component = NonEmpty Fragment

type Pattern = NonEmpty Component

type Parser = Megaparsec.Parsec Void Text

parser :: Parser Pattern
parser =
  (componentParser `NonEmpty.sepBy1` Megaparsec.char '/') <* Megaparsec.eof

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

format :: AudioTrack.AudioTrack -> Pattern -> Text
format = formatWith . flip AudioTrack.format

asText :: Pattern -> Text
asText = formatWith Tag.asText

formatWith :: (Tag.Tag -> Text) -> Pattern -> Text
formatWith formatter pattern =
  fold $ NonEmpty.intersperse "/" $ formatComponentWith formatter <$> pattern

formatComponentWith :: (Tag.Tag -> Text) -> Component -> Text
formatComponentWith formatter = foldMap (formatFragmentWith formatter)

formatFragmentWith :: (Tag.Tag -> Text) -> Fragment -> Text
formatFragmentWith _ (Text text) = text
formatFragmentWith formatter (Placeholder tag) = formatter tag
