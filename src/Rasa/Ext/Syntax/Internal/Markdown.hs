{-# language NamedFieldPuns #-}
module Rasa.Ext.Syntax.Internal.Markdown where

import Rasa.Ext
import Rasa.Ext.Syntax.Internal.Types

import qualified Yi.Rope as Y

import CMarkGFM

lexText :: Y.YiString -> BufAction [(CrdRange,Token)]
lexText txt = do
  return $ lexNodes $ commonmarkToNode [] [] (Y.toText txt)

lexNodes :: Node -> [(CrdRange,Token)]
lexNodes (Node (Nothing) _ _)                 = [] -- Ignore nodes without positions
lexNodes (Node (Just _) (TEXT _) _)           = [] -- Ignore leaf nodes
lexNodes (Node (Just _) DOCUMENT nodes)       = [] -- Skip the root node
  concat $ ( lexNodes <$> nodes )
lexNodes (Node (Just posinfo) nodeType nodes) =
  (posToRange posinfo, nodeTypeToToken nodeType) : ( concat $ ( lexNodes <$> nodes ) )

posToRange :: PosInfo -> CrdRange
posToRange PosInfo{startLine,startColumn,endLine,endColumn} =
  Range
    (Coord (startLine - 1) (startColumn - 1))
    (Coord (endLine - 1) endColumn)

nodeTypeToToken :: NodeType -> Token
nodeTypeToToken (HEADING _)   = MdTitle
nodeTypeToToken BLOCK_QUOTE   = MdQuote
nodeTypeToToken _             = MdOther

setMarkdownStyle :: (CrdRange,Token) -> BufAction (Span CrdRange Style)
setMarkdownStyle (r, MdTitle) = return $ Span r (flair Standout)
setMarkdownStyle (r, MdQuote) = return $ Span r (flair Standout)
setMarkdownStyle (_, _)       = return $ Span (Range (Coord 0 0) (Coord 0 0)) (Style (Nothing, Nothing, Nothing))
