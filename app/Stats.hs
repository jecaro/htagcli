module Stats
  ( CheckErrors (..),
    Stats.empty,
    addTrackErrors,
    addDiscErrors,
    incArtistErrors,
  )
where

data CheckErrors = CheckErrors
  { ceTrackErrors :: Int,
    ceDiscErrors :: Int,
    ceArtistErrors :: Int
  }

empty :: CheckErrors
empty = CheckErrors 0 0 0

addTrackErrors :: Int -> CheckErrors -> CheckErrors
addTrackErrors n errors = errors {ceTrackErrors = ceTrackErrors errors + n}

addDiscErrors :: Int -> CheckErrors -> CheckErrors
addDiscErrors n errors = errors {ceDiscErrors = ceDiscErrors errors + n}

incArtistErrors :: CheckErrors -> CheckErrors
incArtistErrors errors = errors {ceArtistErrors = ceArtistErrors errors + 1}
