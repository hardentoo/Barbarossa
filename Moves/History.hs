{-# LANGUAGE BangPatterns #-}
module Moves.History (
        History, newHist, toHist, valHist
    ) where

import Data.Bits
import qualified Data.Vector.Unboxed.Mutable as V
import Struct.Struct

type History = V.IOVector Int

rows, cols, vsize :: Int
rows = 12
cols = 64
vsize = rows * cols

adr :: Move -> Int
adr m = cols * f + t
    where f | moveColor m == White = e
            | otherwise            = 6 + e
          e = fromEnum $ movePiece m
          t = toSquare m

newHist :: IO History
newHist = V.replicate vsize 0

histw :: Int -> Int -> Int
histw !d !n = (maxl - n') `unsafeShiftR` d
    where maxl = (maxBound `unsafeShiftR` 2) + 1
          !n'  = n `unsafeShiftR` 10

toHist :: History -> Bool -> Move -> Int -> Int -> IO ()
toHist h True  m d n = addHist h (adr m) (histw d n)
toHist h False m d n = subHist h (adr m) (histw d n)

{-# INLINE valHist #-}
valHist :: History -> Move -> IO Int
valHist !h = V.unsafeRead h . adr

addHist :: History -> Int -> Int -> IO ()
addHist h !ad !p = do
    a <- V.unsafeRead h ad
    let !v | a <= lowLimit = lowLimit
           | otherwise     = a - p	-- trick here: we subtract, so that the sort is big to small
    V.unsafeWrite h ad v
    where lowLimit = minBound .|. (minBound `unsafeShiftR` 1)

subHist :: History -> Int -> Int -> IO ()
subHist h !ad !p = do
    a <- V.unsafeRead h ad
    let !v | a >= higLimit = higLimit
           | otherwise     = a + p	-- trick here: we add, so that the sort is big to small
    V.unsafeWrite h ad v
    where higLimit = maxBound `unsafeShiftR` 1
