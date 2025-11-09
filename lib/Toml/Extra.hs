module Toml.Extra (thenValidate) where

import Toml qualified
import Validation qualified

thenValidate ::
  (a -> Validation.Validation [Toml.TomlDecodeError] a) ->
  Toml.TomlCodec a ->
  Toml.TomlCodec a
thenValidate f (Toml.Codec readC writeC) =
  Toml.Codec readThenValidateC writeC
  where
    readThenValidateC toml = case readC toml of
      Validation.Success success -> f success
      Validation.Failure errors -> Validation.Failure errors
