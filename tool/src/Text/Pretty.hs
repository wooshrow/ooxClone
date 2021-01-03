module Text.Pretty(
      module Text.PrettyPrint
    , Pretty(..)
    , dot
    , tab
    , vsep
    , commas
) where

import           Prelude hiding ((<>))
import qualified GHC.Stack as GHC
import           Text.PrettyPrint
import qualified Data.Set as S 
import qualified Data.Map as M
import qualified Data.Stack as T
import qualified Data.HashSet as H
import           Data.List (intersperse)

class Pretty a where
    pretty      :: a -> Doc
    prettyDebug :: a -> Doc
    prettyDebug = pretty
    toString :: a -> String
    toString = render . pretty
    toDebugString :: a -> String
    toDebugString = render . prettyDebug
    prettyPrint :: a -> IO ()
    prettyPrint = putStrLn . toString

instance Pretty Doc where
    pretty = id

instance {-# OVERLAPPING #-} Pretty String where
    pretty = text

instance Pretty Int where
    pretty = int

instance Pretty Float where
    pretty = float

instance Pretty a => Pretty (Maybe a) where
    pretty = maybe (text "Nothing") pretty

instance Pretty a => Pretty (S.Set a) where
    pretty = braces . commas . S.toList
    
instance Pretty a => Pretty (T.Stack a) where
    pretty = brackets . commas . T.toList

instance Pretty a => Pretty (H.HashSet a) where
    pretty = braces . commas . H.toList

instance {-# OVERLAPPABLE #-} Pretty a => Pretty [a] where
    pretty = brackets . commas

instance (Pretty k, Pretty v) => Pretty (M.Map k v) where
    pretty = brackets . commas . map prettyElem . M.toList
        where
            prettyElem (k, v) = pretty k <> pretty "->" <> pretty v

instance Pretty GHC.CallStack where
    pretty = text . GHC.prettyCallStack

dot :: Doc
dot = char '.'

tab :: Doc -> Doc
tab = nest 4

vsep :: Pretty a => [a] -> Doc
vsep = foldr (($+$) . pretty) empty

commas :: Pretty a => [a] -> Doc
commas = hcat . intersperse comma . map pretty