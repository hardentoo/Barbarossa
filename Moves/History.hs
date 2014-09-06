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
vsize = rows * cols * 2	-- one plane per player!

-- From the absolute depth we know which playes is the move for
-- odd: we, even: you, so we can decide to which plane to go
adr :: Int -> Word32 -> Int
adr !d !w = ((d .&. 1) `unsafeShiftL` 12) .|. fromIntegral (w .&. 0xFFF)

newHist :: IO History
newHist = V.replicate vsize 0

hisVal :: Int -> Int32
hisVal = unsafeShiftL hv
    where hv = 1 `unsafeShiftL` (maxd + 1)
          maxd = 20

toHist :: History -> Int -> Move -> IO ()
toHist h !d (Move w) = addHist h (adr d w) $ hisVal d

addHist :: History -> Int -> Int32 -> IO ()
addHist h !ad !p = do
    a <- V.unsafeRead h ad
    let !u = a - p	-- trick here: we subtract, so that the sort is big to small
        !v = if u >= 0 then minBound else u	-- overflow!
    V.unsafeWrite h ad v

valHist :: History -> Int -> Move -> IO Int32
valHist !h !d (Move w) = V.unsafeRead h $! adr d w
