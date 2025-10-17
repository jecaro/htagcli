module Data.List.NonEmpty.Extra (unsnoc) where

import Data.List.NonEmpty qualified as NonEmpty

unsnoc :: NonEmpty a -> ([a], a)
unsnoc list = (rest, lastElement)
  where
    (lastElement :| restReversed) = NonEmpty.reverse list
    rest = reverse restReversed
