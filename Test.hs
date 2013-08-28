{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import qualified NaiveBitBoard as NB
import qualified Reversi.Tatsuki.BitBoard as TB

import System.Random (Random)
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Gen

import Control.Applicative
import Data.Bits
import Data.Word

newtype BP = BP { unwrapBP :: (Int, (Word64, Word64)) }
           deriving (Eq, Show)
instance Arbitrary BP where
  arbitrary = BP <$> suchThat gen f
    where
      f (pos, (pla, opp)) = (pla .|. opp) .&. 1 `shiftL` pos == 0
      gen = (,) <$> pGen <*> bGen
      pGen  = choose (0, 63)
      bGen = unwrapBoard <$> arbitrary

newtype B = B { unwrapBoard :: (Word64, Word64) }
          deriving (Eq, Show)
instance Arbitrary B where
  arbitrary = B . toBitBoard <$> choose (0, 3^64 - 1)

toBitBoard :: Integer -> (Word64, Word64)
toBitBoard = toBitBoard' (0, 0) 0

toBitBoard' acc 64 _ = acc
toBitBoard' (pla, opp) i n = toBitBoard' acc' (i+1) n'
  where
    (n', x) = n `divMod` 3
    acc' =  case x of
      1 -> (1 `shiftL` i .|. pla, opp)
      2 -> (pla, 1 `shiftL` i .|. opp)
      _ -> (pla, opp)

-- defining prettyprint in bitboard module may be better
showHemiBoard :: Word64 -> String
showHemiBoard set = unlines $ flip map [7,6..0] $ \x -> flip map [7,6..0] $ \y -> if set .&. 1 `shiftL` (x `shiftL` 3 .|. y) /= 0 then 'o' else '-'

prop_admissible (B brd) = NB.admissible brd == TB.admissible brd

prop_flipped (BP (pos, brd)) = NB.flipped pos brd == TB.flipped pos brd
