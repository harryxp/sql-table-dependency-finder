module Dependencies (Dependencies(..)) where

import Data.List (intercalate)
import Data.Monoid ((<>))
import Data.Ord (comparing)

-- TODO use a sorted collection type instead of []
data Dependencies = Dependencies String [String]

instance Eq Dependencies where
  (Dependencies t1 d1) == (Dependencies t2 d2) = t1 == t2 && d1 == d2 -- assumption: d1 and d2 are already sorted

instance Ord Dependencies where
  compare (Dependencies t1 d1) (Dependencies t2 d2) = compare t1 t2
                                                   <> comparing length d1 d2
                                                   <> compare d1 d2   -- assumption: d1 and d2 are already sorted

instance Show Dependencies where
  show (Dependencies tbl deps) = intercalate "\n" (tbl : (("  "++) <$> deps))

