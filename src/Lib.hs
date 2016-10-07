{-# language DeriveDataTypeable, DeriveGeneric #-}
{-# language TemplateHaskell #-}
module Lib
    -- (
    --   ourAdd
    -- )
    where



import Text.Printf
import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)



-- | Add two 'Int' values.
ourAdd :: Int  -- ^ left
       -> Int  -- ^ right
       -> Int  -- ^ sum
ourAdd x y = x + y
