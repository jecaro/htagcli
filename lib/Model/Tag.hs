module Model.Tag (Tag (..), asText, parser) where

import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char qualified as Megaparsec

type Parser = Megaparsec.Parsec Void Text

data Tag = Title | Artist | Album | AlbumArtist | Genre | Year | Track | Disc
  deriving (Show, Eq, Enum, Bounded)

asText :: Tag -> Text
asText Title = "title"
asText Artist = "artist"
asText Album = "album"
asText AlbumArtist = "albumartist"
asText Genre = "genre"
asText Year = "year"
asText Track = "track"
asText Disc = "disc"

parser :: Parser Tag
parser =
  Megaparsec.choice
    [ Title <$ Megaparsec.string "title",
      Artist <$ Megaparsec.string "artist",
      AlbumArtist <$ Megaparsec.string "albumartist",
      Album <$ Megaparsec.string "album",
      Genre <$ Megaparsec.string "genre",
      Year <$ Megaparsec.string "year",
      Track <$ Megaparsec.string "track",
      Disc <$ Megaparsec.string "disc"
    ]
