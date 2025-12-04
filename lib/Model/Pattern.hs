{-# LANGUAGE QuasiQuotes #-}

module Model.Pattern
  ( Formatting (..),
    Padding (..),
    Component,
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

import Control.Applicative.Combinators.NonEmpty qualified as NonEmpty
import Data.Foldable qualified as Foldable
import Data.List.NonEmpty.Extra qualified as NonEmpty
import Data.Text qualified as Text
import Model.AudioTrack qualified as AudioTrack
import Model.Tag qualified as Tag
import Path ((</>))
import Path qualified
import Sound.HTagLib qualified as HTagLib
import Sound.HTagLib.Extra qualified as HTagLib
import System.FilePath qualified as FilePath
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char qualified as Megaparsec
import Text.Printf qualified as Text

data Placeholder
  = PlTag Tag.Tag
  | PlTagOr Tag.Tag Tag.Tag
  | PlOptional Text Tag.Tag Text
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
    foPadTrackNumbers :: Padding,
    foPadDiscNumbers :: Padding,
    foPlaceholderMaxLength :: Int
  }
  deriving (Show, Eq)

noFormatting :: Formatting
noFormatting =
  Formatting
    { foCharActions = [('/', ChRemove)],
      foPadTrackNumbers = Pad 0,
      foPadDiscNumbers = Pad 0,
      foPlaceholderMaxLength = 30
    }

parser :: Parser Pattern
parser =
  (componentParser `NonEmpty.sepBy1` Megaparsec.char '/') <* Megaparsec.eof

tags :: Pattern -> [Tag.Tag]
tags = foldMap $ foldMap tagList
  where
    tagList (FrPlaceholder (PlTag tag)) = [tag]
    tagList (FrPlaceholder (PlTagOr _ fallback)) = [fallback]
    tagList _ = []

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
    (Megaparsec.try optionalTag <|> Megaparsec.try tagOr <|> tag)
  where
    tag = PlTag <$> Tag.parser
    tagOr =
      PlTagOr
        <$> (Tag.parser <* Megaparsec.char '|')
        <*> Tag.parser
    optionalTag =
      PlOptional
        <$> Megaparsec.takeWhileP Nothing (\c -> c /= '?' && c /= '}')
        <*> Megaparsec.between
          (Megaparsec.char '?')
          (Megaparsec.char '?')
          Tag.parser
        <*> Megaparsec.takeWhileP Nothing (/= '}')

format :: Formatting -> AudioTrack.AudioTrack -> Pattern -> Maybe Text
format formatting audioTrack pattern =
  toText . Path.toFilePath <$> toPath formatting audioTrack pattern

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
      -- If the formatted component is empty, we skip it, that can happen if
      -- tags are not presents
      case formatComponentWith formatter component of
        "" -> Just dir
        nonEmptyText -> (dir </>) <$> Path.parseRelDir (toString nonEmptyText)

asText :: Pattern -> Text
asText = formatWith placeholderAsText

placeholderAsText :: Placeholder -> Text
placeholderAsText (PlTag tag) = "{" <> Tag.asText tag <> "}"
placeholderAsText (PlTagOr tag fallback) =
  "{" <> Tag.asText tag <> "|" <> Tag.asText fallback <> "}"
placeholderAsText (PlOptional before tag after) =
  "{" <> before <> "?" <> Tag.asText tag <> "?" <> after <> "}"

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
  fold $
    intersperse "/" $
      -- Remove empty components
      filter (not . Text.null) $
        toList $
          formatComponentWith formatter <$> pattern

formatComponentWith :: (Placeholder -> Text) -> Component -> Text
formatComponentWith formatter = foldMap (formatFragmentWith formatter)

componentToRelFile ::
  (Placeholder -> Text) -> Component -> Maybe (Path.Path Path.Rel Path.File)
componentToRelFile formatter =
  Path.parseRelFile . (toString . formatComponentWith formatter)

formatFragmentWith :: (Placeholder -> Text) -> Fragment -> Text
formatFragmentWith _ (FrText text) = text
formatFragmentWith formatter (FrPlaceholder placeholder) = formatter placeholder

formatPlaceholder :: Formatting -> AudioTrack.AudioTrack -> Placeholder -> Text
formatPlaceholder formatting track (PlTag tag) = formatTag formatting track tag
formatPlaceholder formatting track (PlTagOr tag fallback)
  | AudioTrack.haveTag tag track = formatTag formatting track tag
  | otherwise = formatTag formatting track fallback
formatPlaceholder formatting track (PlOptional before tag after)
  | AudioTrack.haveTag tag track =
      before <> formatTag formatting track tag <> after
  | otherwise = ""

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
formatTag Formatting {..} AudioTrack.AudioTrack {..} Tag.Track =
  maybe "" (numberFormat foPadTrackNumbers . HTagLib.unTrackNumber) atTrack
formatTag Formatting {..} AudioTrack.AudioTrack {..} Tag.Disc =
  maybe "" (numberFormat foPadDiscNumbers . HTagLib.unDiscNumber) atDisc

textFormatter :: Formatting -> Text -> Text
textFormatter Formatting {..} = Text.take foPlaceholderMaxLength . applyUnwanted
  where
    applyUnwanted = foldr ((.) . unwanted) id foCharActions
    unwanted :: (Char, CharAction) -> Text -> Text
    unwanted (char, ChRemove) = Text.filter (/= char)
    unwanted (char, ChReplace with) = Text.map (replace char with)
    replace char with current
      | current == char = with
      | otherwise = current

numberFormat :: Padding -> Int -> Text
numberFormat padding =
  Text.pack . Text.printf ("%0" <> show paddingOrZero <> "d")
  where
    paddingOrZero
      | Pad n <- padding = n
      | otherwise = 0

match :: Formatting -> AudioTrack.AudioTrack -> Pattern -> FilePath -> Bool
match formatting track pattern filename =
  length allComponents >= length pattern
    -- We start from the right to make it easier to handle the case where the
    -- pattern has not the same number of components as the path
    && fst (foldr walk (True, reverse allComponents) pattern)
  where
    walk :: Component -> (Bool, [Text]) -> (Bool, [Text])
    walk _ (True, []) = (False, [])
    walk component (True, path : paths) =
      case matchComponent formatting track component path of
        -- If the formatted component is empty we don't consume a path component
        (Just "") -> (True, path : paths)
        (Just _) -> (True, paths)
        _ -> (False, path : paths)
    walk _ acc = acc
    withoutExtension = FilePath.dropExtension filename
    allComponents = toText <$> FilePath.splitPath withoutExtension

matchComponent ::
  Formatting ->
  AudioTrack.AudioTrack ->
  Component ->
  Text ->
  -- | If matched, return the formatted component
  Maybe Text
matchComponent formatting track component text =
  fst $ foldl' matchFragment' (Just "", text) component
  where
    matchFragment' :: (Maybe Text, Text) -> Fragment -> (Maybe Text, Text)
    matchFragment' (Just formatted, remaining) fragment =
      case matchFragment formatting track fragment remaining of
        (formatted', Just rest) -> (Just (formatted <> formatted'), rest)
        (_, Nothing) -> (Nothing, remaining)
    matchFragment' acc _ = acc

matchFragment ::
  Formatting ->
  AudioTrack.AudioTrack ->
  Fragment ->
  Text ->
  -- | Return the formatted fragment and the remaining text if matched
  (Text, Maybe Text)
matchFragment formatting track fragment txt =
  (formatted, Text.stripPrefix formatted $ ignoreLeadingZeros txt)
  where
    formatted = formatFragmentWith (formatPlaceholder formatting track) fragment
    ignoreLeadingZeros
      | FrPlaceholder (PlTag Tag.Track) <- fragment,
        Ignore <- foPadTrackNumbers formatting =
          Text.dropWhile (== '0')
      | otherwise = id
