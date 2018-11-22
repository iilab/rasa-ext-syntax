{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}
{-# language ExistentialQuantification #-}
{-# language ScopedTypeVariables #-}
{-# language RankNTypes #-}
{-# language StandaloneDeriving #-}

module Rasa.Ext.Syntax
  ( syntax
  , Mapper(..)
  , Lexer(..)
  , Styler(..)
  , Syntax(..)
  , Tokens(..)
  , TokenRange(..)
  ) where

import Rasa.Ext
import Rasa.Ext.Files
import Rasa.Ext.Views

import Data.Default
import Data.Typeable

import Data.Text (Text(..))
import Control.Monad
import qualified Yi.Rope as Y

type Lexer a = Y.YiString -> Tokens a
type Styler a = TokenRange a -> BufAction (Span CrdRange Style)
type Mapper = [Extension]
data Syntax a = Syntax String Mapper (Lexer a) (Styler a)

-- instance (Show a, Default a, Typeable a) => Default (Syntax a) where
--   def = Syntax (\_ -> Tokens []) (\_ -> do { return (Span (Range (Coord 0 0) (Coord 0 0)) def) } )

-- | Stores the token ranges and types in each buffer.
data TokenRange a = (Default a, Typeable a) => TokenRange (CrdRange, a)
  deriving (Typeable)

instance (Show a, Default a, Typeable a) => Show (TokenRange a) where
  show (TokenRange (range, a)) = "TokenRange (" ++ show range ++ "," ++ show a ++ ")"

instance (Show a, Default a, Typeable a) => Default (TokenRange a) where
  def = TokenRange (Range (Coord 0 0) (Coord 0 0), def)

data Tokens a = (Show a, Default a, Typeable a) => Tokens
  { unToken :: [TokenRange a] }

deriving instance (Show a, Default a, Typeable a) => Show (Tokens a)
deriving instance (Show a, Default a, Typeable a) => Typeable (Tokens a)

instance (Show a, Default a, Typeable a) => Default (Tokens a) where
  def = Tokens []


-- Each syntax extension would add a BufferSyntax MySyntax type.
-- And a handler for EditorBuffer (BufferSyntax MySyntax)
-- It would need to interact with the File extension:
--   It would need to add a mapping from a file .ext to a Syntax
-- So the File extension needs to send a ".ext" Event

-- Syntax API
--   A Syntax Extension has
--

syntaxName :: String -> BufAction (RenderInfo)
syntaxName name =  do
  return $ styleText (Y.fromString $ name) $ fg Blue

handleFileLoaded :: (Show a, Default a, Typeable a) => String -> Mapper -> Lexer a -> Styler a -> FileLoaded -> App ()
handleFileLoaded name mapper lexer styler (FileLoaded ext bufRef)
  | elem ext mapper = bufDo_ bufRef $ do
      void . addBottomStatus $ syntaxName name
      setHighlightProvider styler
      txt <- getText
      lexText txt lexer >>= setBufExt

  | otherwise = return ()
handleFileLoaded _ _ _ _ _ = return ()

syntax :: (Show a, Default a, Typeable a) => [Syntax a] -> App ()
syntax = mapM_ (\(Syntax name mapper lexer styler) -> do
  void . onFileLoaded $ handleFileLoaded name mapper lexer styler)

  -- onBufAdded_ $
  --   -- Setup the Highlighter and do an initial parse
  --   \(BufAdded bufRef) -> bufDo_ bufRef $ do
  --     setHighlightProvider styler
  --     txt <- getText
  --     lexText txt lexer >>= setBufExt
  -- onEveryNewBuffer_ $ do
  --   void . onBufTextChanged $ startParse lexer )

lexText ::  (Show a, Default a, Typeable a) => Y.YiString -> Lexer a -> BufAction (Tokens a)
lexText txt lexer = do
  -- logInfo $ "lexText: " ++ ( show $ lexer [] [] (Y.toText txt) )
  return $ lexer txt

startParse ::  (Show a, Default a, Typeable a) => Lexer a -> BufTextChanged ->  BufAction ()
startParse lexer _ = do
    txt <- getText
    lexText txt lexer >>= setBufExt

-- We used to filter, we probably need to bring this back with a universal UnrecognizedToken

-- emptyTokens :: (CrdRange,Token) -> Bool
-- emptyTokens (r, MdOther) = False
-- emptyTokens (_, _) = True

-- | Get the list of ranges
getRanges ::  (Show a, Default a, Typeable a) => BufAction (Tokens a)
getRanges = do
  ranges <- getBufExt
  -- logInfo $ "getRanges: " ++ ( show $ filter emptyTokens ranges )
  return ranges
  -- return $ filter emptyTokens ranges

-- | Sequences actions over each range as a 'BufAction'
tokensDo ::  (Show a, Default a, Typeable a) => Styler a -> BufAction [Span CrdRange Style]
-- tokensDo f = getRanges >>= mapM f
tokensDo f = do
  Tokens ranges <- getRanges
  done <- mapM f ranges
  -- logInfo $ show done
  return done

-- | Adds highlight specific styles
setHighlightProvider ::  (Show a, Default a, Typeable a) => Styler a -> BufAction ()
setHighlightProvider styler = do
  void . addStyleProvider $ tokensDo styler
