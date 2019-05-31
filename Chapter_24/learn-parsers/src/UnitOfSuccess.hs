module UnitOfSuccess where

import Text.Trifecta

myFunc :: Parser Integer
myFunc = do
    myInt <- integer
    eof
    return myInt