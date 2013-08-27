-- | This module exports a quasiquoter for binary integer literals.
--
-- Example usage:
--
-- @
-- import Language.Literals.Binary
-- import Data.Word
--
-- not :: Word32 -> Word32
-- not [b| 0 |] = [b| 1 |]
-- not [b| 1 |] = [b| 0 |]
-- @

module Language.Literals.Binary where

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Data.Bits


b = QuasiQuoter
  { quoteExp  = return . LitE . IntegerL . readBinary
  , quotePat  = return . LitP . IntegerL . readBinary
  , quoteType = error "No quasiquoter for types."
  , quoteDec  = error "No quasiquoter for declarations." }

readBinary :: String -> Integer
readBinary = foldl f 0 where
  f x '0' = shift x 1
  f x '1' = shift x 1 + 1
  f x ' ' = x
  f x _   = error "Not a valid binary literal."
