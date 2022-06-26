{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils where

import Data.Text (Text)
import Data.Hashable

import Syntax
import Witch

hashRettyName :: Var -> Var -> Text
hashRettyName baseName var =
  let
    hashVal = from @String $ show $ hash $ baseName <> var
  in
    baseName <> "_" <> hashVal
