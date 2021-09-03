module Data.CryptoCurrency.Types.Conversion where

-- Describes a conversion from one coin to another

import Data.Time (Day)

import Data.XHTML (Name)

data ConvertData =
   CD Day Name Name Double Name Double Double Double Double (Maybe Name)
      deriving (Eq, Ord, Show)

-- now we convert the above to a db-storable representation
-- (and have, of course, the dual)

toConvert' :: LookupTable -> LookupTable -> ConvertData -> Maybe Convert'
toConvert' portLk coinLk (CD dt port c1 amt1 c2 amt2 fee comm tx confirm) =
   
