module Tag (Tag (..), asText, parser) where

import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char qualified as Megaparsec

type Parser = Megaparsec.Parsec Void Text

data Tag = Title | Artist | Album | Genre | Year | Track
  deriving (Show, Eq, Enum, Bounded)

asText :: Tag -> Text
asText Title = "title"
asText Artist = "artist"
asText Album = "album"
asText Genre = "genre"
asText Year = "year"
asText Track = "track"

parser :: Parser Tag.Tag
parser =
  Megaparsec.choice
    [ Tag.Title <$ Megaparsec.string "title",
      Tag.Artist <$ Megaparsec.string "artist",
      Tag.Album <$ Megaparsec.string "album",
      Tag.Genre <$ Megaparsec.string "genre",
      Tag.Year <$ Megaparsec.string "year",
      Tag.Track <$ Megaparsec.string "track"
    ]
