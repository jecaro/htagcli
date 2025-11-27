module Stats
  ( CheckErrors (..),
    Stats.empty,
    addTrackErrors,
    addAlbumErrors,
    incArtistErrors,
  )
where

data CheckErrors = CheckErrors
  { ceTrackErrors :: Int,
    ceAlbumErrors :: Int,
    ceArtistErrors :: Int
  }

empty :: CheckErrors
empty = CheckErrors 0 0 0

addTrackErrors :: Int -> CheckErrors -> CheckErrors
addTrackErrors n errors = errors {ceTrackErrors = ceTrackErrors errors + n}

addAlbumErrors :: Int -> CheckErrors -> CheckErrors
addAlbumErrors n errors = errors {ceAlbumErrors = ceAlbumErrors errors + n}

incArtistErrors :: CheckErrors -> CheckErrors
incArtistErrors errors = errors {ceArtistErrors = ceArtistErrors errors + 1}
