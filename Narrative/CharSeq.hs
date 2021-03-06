module Narrative.CharSeq where

import Data.String
import Data.Text as T
import Data.Text.Lazy as L
import Data.Monoid

class (IsString a, Monoid a) => CharSeq a where
  toString :: a -> String

instance CharSeq T.Text where
  toString = T.unpack

instance CharSeq L.Text where
  toString = L.unpack

instance CharSeq String where
  toString = id
