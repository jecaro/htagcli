{-# LANGUAGE QuasiQuotes #-}

module Pattern
  ( Formatting (..),
    Padding (..),
    parsePadding,
    paddingAsText,
    Fragment (..),
    Placeholder (..),
    CharAction (..),
    charActionAsText,
    charActionParser,
    addSlashIfNeeded,
    Pattern,
    asText,
    parser,
    noFormatting,
    format,
    tags,
    match,
    toPath,
  )
where

import AudioTrack qualified
import Control.Applicative.Combinators.NonEmpty qualified as NonEmpty
import Data.Foldable qualified as Foldable
import Data.List.NonEmpty qualified as NonEmpty
import Data.List.NonEmpty.Extra qualified as NonEmpty
import Data.Text qualified as Text
import Path ((</>))
import Path qualified
import Sound.HTagLib qualified as HTagLib
import Sound.HTagLib.Extra qualified as HTagLib
import System.FilePath qualified as FilePath
import Tag qualified
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char qualified as Megaparsec
import Text.Printf qualified as Text

data Placeholder = PlTag Tag.Tag | PlAlbumArtist
  deriving (Eq, Show)

data Fragment = FrText Text | FrPlaceholder Placeholder
  deriving (Eq, Show)

type Component = NonEmpty Fragment

type Pattern = NonEmpty Component

type Parser = Megaparsec.Parsec Void Text

data CharAction = ChRemove | ChReplace Char
  deriving (Show, Eq)

data Padding
  = Ignore
  | Pad Int
  deriving (Show, Eq)

data Formatting = Formatting
  { foCharActions :: [(Char, CharAction)],
    foPadTrackNumbers :: Padding
  }
  deriving (Show, Eq)

noFormatting :: Formatting
noFormatting =
  Formatting
    { foCharActions = [('/', ChRemove)],
      foPadTrackNumbers = Pad 0
    }

parser :: Parser Pattern
parser =
  (componentParser `NonEmpty.sepBy1` Megaparsec.char '/') <* Megaparsec.eof

tags :: Pattern -> [Tag.Tag]
tags = foldMap $ foldMap tagList
  where
    tagList (FrPlaceholder (PlTag tag)) = [tag]
    tagList (FrPlaceholder PlAlbumArtist) = [Tag.Artist]
    tagList (FrText _) = []

componentParser :: Parser Component
componentParser = NonEmpty.some fragmentParser

fragmentParser :: Parser Fragment
fragmentParser = FrPlaceholder <$> placeholderParser <|> FrText <$> textParser

textParser :: Parser Text
textParser = toText <$> Megaparsec.some (Megaparsec.satisfy notSlashNorBrace)
  where
    notSlashNorBrace c = c /= '/' && c /= '{' && c /= '}'

placeholderParser :: Parser Placeholder
placeholderParser =
  Megaparsec.between
    (Megaparsec.char '{')
    (Megaparsec.char '}')
    (PlAlbumArtist <$ Megaparsec.string "albumartist_" <|> PlTag <$> Tag.parser)

format :: Formatting -> AudioTrack.AudioTrack -> Pattern -> Text
format formatting = formatWith . formatPlaceholder formatting

toPath ::
  Formatting ->
  AudioTrack.AudioTrack ->
  Pattern ->
  Maybe (Path.Path Path.Rel Path.File)
toPath formatting track@AudioTrack.AudioTrack {..} pattern = do
  formattedFile <- toPathWith (formatPlaceholder formatting track) pattern
  -- We do not handle files without extensions
  extension <- Path.fileExtension atFile
  Path.addExtension extension formattedFile

toPathWith ::
  (Placeholder -> Text) -> Pattern -> Maybe (Path.Path Path.Rel Path.File)
toPathWith formatter pattern = (</>) <$> mbFormattedDir <*> mbFormattedFile
  where
    (dirComponents, fileComponent) = NonEmpty.unsnoc pattern
    mbFormattedDir =
      Foldable.foldlM appendComponent [Path.reldir|.|] dirComponents
    mbFormattedFile = componentToRelFile formatter fileComponent
    appendComponent dir component =
      (dir </>) <$> componentToRelDir formatter component

asText :: Pattern -> Text
asText = formatWith placeholderAsText

placeholderAsText :: Placeholder -> Text
placeholderAsText (PlTag tag) = "{" <> Tag.asText tag <> "}"
placeholderAsText PlAlbumArtist = "{albumartist_}"

charActionAsText :: CharAction -> Text
charActionAsText ChRemove = "remove"
charActionAsText (ChReplace c) = "replace:" <> Text.singleton c

charActionParser :: Parser CharAction
charActionParser =
  ChRemove
    <$ Megaparsec.string "remove"
      <|> ChReplace
    <$> (Megaparsec.string "replace:" *> Megaparsec.anySingle)

addSlashIfNeeded :: [(Char, CharAction)] -> [(Char, CharAction)]
addSlashIfNeeded charToCharActions
  | any ((== '/') . fst) charToCharActions = charToCharActions
  | otherwise = ('/', ChRemove) : charToCharActions

paddingAsText :: Padding -> Text
paddingAsText Ignore = "ignore"
paddingAsText (Pad n) = show n

parsePadding :: Text -> Either Text Padding
parsePadding "ignore" = Right Ignore
parsePadding text =
  case readMaybe (Text.unpack text) of
    Just n | n > 0 -> Right (Pad n)
    _ -> Left "Should be 'none' or a positive integer"

formatWith :: (Placeholder -> Text) -> Pattern -> Text
formatWith formatter pattern =
  fold $ NonEmpty.intersperse "/" $ formatComponentWith formatter <$> pattern

formatComponentWith :: (Placeholder -> Text) -> Component -> Text
formatComponentWith formatter = foldMap (formatFragmentWith formatter)

componentToRelDir ::
  (Placeholder -> Text) -> Component -> Maybe (Path.Path Path.Rel Path.Dir)
componentToRelDir formatter =
  Path.parseRelDir . (toString . formatComponentWith formatter)

componentToRelFile ::
  (Placeholder -> Text) -> Component -> Maybe (Path.Path Path.Rel Path.File)
componentToRelFile formatter =
  Path.parseRelFile . (toString . formatComponentWith formatter)

formatFragmentWith :: (Placeholder -> Text) -> Fragment -> Text
formatFragmentWith _ (FrText text) = text
formatFragmentWith formatter (FrPlaceholder placeholder) = formatter placeholder

formatPlaceholder :: Formatting -> AudioTrack.AudioTrack -> Placeholder -> Text
formatPlaceholder formatting track (PlTag tag) = formatTag formatting track tag
formatPlaceholder formatting track PlAlbumArtist = formatTag formatting track tag
  where
    tag
      | not $
          Text.null $
            HTagLib.unAlbumArtist (AudioTrack.atAlbumArtist track) =
          Tag.AlbumArtist
      | otherwise = Tag.Artist

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
textFormatter Formatting {..} = applyUnwanted
  where
    applyUnwanted = foldr ((.) . unwanted) id foCharActions
    unwanted :: (Char, CharAction) -> Text -> Text
    unwanted (char, ChRemove) = Text.filter (/= char)
    unwanted (char, ChReplace with) = Text.map (replace char with)
    replace char with current
      | current == char = with
      | otherwise = current

trackNumberFormat :: Formatting -> Int -> Text
trackNumberFormat Formatting {..} =
  Text.pack . Text.printf ("%0" <> show padding <> "d")
  where
    padding
      | Pad n <- foPadTrackNumbers = n
      | otherwise = 0

match :: Formatting -> AudioTrack.AudioTrack -> Pattern -> FilePath -> Bool
match formatting track pattern filename =
  length allComponents >= length pattern
    && all (uncurry $ matchComponent formatting track) patternAndPathComponents
  where
    withoutExtension = FilePath.dropExtension filename
    allComponents = toText <$> FilePath.splitPath withoutExtension
    components = drop (lengthComponents - lengthPattern) allComponents
    lengthComponents = length allComponents
    lengthPattern = length pattern
    patternAndPathComponents = zip (NonEmpty.toList pattern) components

matchComponent ::
  Formatting -> AudioTrack.AudioTrack -> Component -> Text -> Bool
matchComponent formatting track component text =
  fst $ foldl' matchFragment' (True, text) component
  where
    matchFragment' (True, remaining) fragment =
      case matchFragment formatting track fragment remaining of
        Just rest -> (True, rest)
        Nothing -> (False, remaining)
    matchFragment' acc _ = acc

matchFragment ::
  Formatting -> AudioTrack.AudioTrack -> Fragment -> Text -> Maybe Text
matchFragment formatting track fragment =
  Text.stripPrefix formatted . ignoreLeadingZeros
  where
    formatted = formatFragmentWith (formatPlaceholder formatting track) fragment
    ignoreLeadingZeros
      | FrPlaceholder (PlTag Tag.Track) <- fragment,
        Ignore <- foPadTrackNumbers formatting =
          Text.dropWhile (== '0')
      | otherwise = id
