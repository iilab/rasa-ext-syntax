{-# Language OverloadedStrings, TemplateHaskell #-}

module Rasa.Ext.Syntax where

import Rasa.Ext

import Control.Monad

import Rasa.Ext.Syntax.Internal.Types
import Rasa.Ext.Syntax.Internal.Markdown

syntax :: App ()
syntax = do
  onBufAdded_ $
    -- Setup the Highlighter and do an initial parse
    \(BufAdded bufRef) -> bufDo_ bufRef $ do
      setHighlightProvider
      txt <- getText
      lexText txt >>= setBufExt . Tokens
  onEveryNewBuffer_ $ do
    void . onBufTextChanged $ startParse

startParse :: BufTextChanged -> BufAction ()
startParse (BufTextChanged _ _) = do
    txt <- getText
    lexText txt >>= setBufExt . Tokens

-- | Get the list of ranges
getRanges :: BufAction [(CrdRange,Token)]
getRanges = do
  Tokens ranges <- getBufExt
  return ranges

-- | Sequences actions over each range as a 'BufAction'
tokensDo :: ((CrdRange, Token) -> BufAction a) -> BufAction [a]
tokensDo f = getRanges >>= mapM f

-- | Adds highlight specific styles
setHighlightProvider :: BufAction ()
setHighlightProvider = void . addStyleProvider $ tokensDo setMarkdownStyle
