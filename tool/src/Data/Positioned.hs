{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Positioned(
      Positioned(..)
    , WithPos(..) 
    , unknownPos
    , module Text.Parsec.Pos
) where

import Text.Parsec.Pos
import Text.Pretty

unknownPos :: SourcePos
unknownPos = newPos "" (-1) (-1)

data Positioned a 
    = Positioned { pos :: SourcePos, value :: a }
    deriving (Show)

class WithPos a where
    getPos :: a -> SourcePos

instance WithPos (Positioned a) where
    getPos Positioned{pos} = pos

instance Pretty SourcePos where
    pretty pos = pFile (sourceName pos) <+> pLine (sourceLine pos) <+> pColumn (sourceColumn pos)
        where
            pFile file = text "in file" <+> quotes (pretty file)
            pLine line = text "at line" <+> quotes (pretty line)
            pColumn column = text "and column" <+> quotes (pretty column)
