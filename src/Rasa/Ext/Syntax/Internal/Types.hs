module Rasa.Ext.Syntax.Internal.Types where

import Data.Default
import Data.Typeable
import Rasa.Ext

data Token =
  MdTitle | MdBold | MdEmph | MdQuote | MdOther
  deriving (Typeable, Show)

-- | Stores the token ranges and types in each buffer.
data Tokens =
  Tokens [(CrdRange,Token)]
  deriving (Typeable, Show)

instance Default Tokens where
  def = Tokens [(Range (Coord 0 0) (Coord 0 0), MdOther)]
