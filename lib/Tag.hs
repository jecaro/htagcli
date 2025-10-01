module Tag (Tag (..), render, parse) where

data Tag = Title | Artist | Album | Genre | Year | Track
  deriving (Show, Eq)

render :: Tag -> Text
render Title = "title"
render Artist = "artist"
render Album = "album"
render Genre = "genre"
render Year = "year"
render Track = "track"

parse :: String -> Maybe Tag
parse "title" = Just Title
parse "artist" = Just Artist
parse "album" = Just Album
parse "genre" = Just Genre
parse "year" = Just Year
parse "track" = Just Track
parse _ = Nothing
