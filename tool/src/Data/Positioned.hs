{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Positioned(
      Position(..)
    , Positioned(..)
    , WithPos(..)
    , newPos
    , unknownPos
    , sourcePosToPosition
    , P.SourceName
    , P.Line
    , P.Column
) where

import           Text.Pretty
import qualified Text.Parsec.Pos as P

data Position
    = SourcePos { sourceFile :: P.SourceName, sourceLine :: P.Line, sourceColumn :: P.Column }
    deriving (Show, Eq, Ord)

newPos :: P.SourceName -> P.Line -> P.Column -> Position
newPos = SourcePos

unknownPos :: Position
unknownPos = SourcePos "" (-1) (-1)

sourcePosToPosition :: P.SourcePos -> Position
sourcePosToPosition pos = SourcePos 
    { sourceFile   = P.sourceName pos
    , sourceLine   = P.sourceLine pos
    , sourceColumn = P.sourceColumn pos }
        
data Positioned a 
    = Positioned { pos :: Position, value :: a }
    deriving (Show)

class WithPos a where
    getPos :: a -> Position

instance WithPos (Positioned a) where
    getPos Positioned{pos} = pos

instance Pretty Position where
    pretty pos = pFile (sourceFile pos) <+> pLine (sourceLine pos) <+> pColumn (sourceColumn pos)
        where
            pFile file = text "in file" <+> quotes (pretty file)
            pLine line = text "at line" <+> quotes (pretty line)
            pColumn column = text "and column" <+> quotes (pretty column)
