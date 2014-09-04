{-# LANGUAGE BangPatterns #-}
module Moves.History (
        History, newHist, toHist, valHist
    ) where

import Data.Bits
import Data.Int
import qualified Data.Vector.Unboxed.Mutable as V
import Data.Word
import Struct.Struct

type History = V.IOVector Int32

rows, cols, vsize :: Int
rows = 64
cols = 64
vsize = rows * cols

adr :: Word32 -> Int
adr = fromIntegral . (.&. 0xFFF)

newHist :: IO History
newHist = V.replicate vsize 0

histw :: Int -> Int32
histw = unsafeShiftL hv
    where hv = 1 `unsafeShiftL` (maxd + 1)
          maxd = 20

toHist :: History -> Move -> Int -> IO ()
toHist h (Move w) = addHist h (adr w) . histw

addHist :: History -> Int -> Int32 -> IO ()
addHist h !ad !p = do
    a <- V.unsafeRead h ad
    let !u = a - p	-- trick here: we subtract, so that the sort is big to small
        !v = if u >= 0 then minBound else u	-- overflow!
    V.unsafeWrite h ad v

valHist :: History -> Move -> IO Int32
valHist !h (Move w) = V.unsafeRead h $! adr w
