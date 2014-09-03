{-# LANGUAGE BangPatterns #-}
module Moves.History (
        History, newHist, toHist, valHist
    ) where

import Data.Bits
import qualified Data.Vector.Unboxed.Mutable as V
import Struct.Struct

type History = V.IOVector Int

rows, cols, vsize :: Int
rows = 64
cols = 64
vsize = rows * cols

adr :: Int -> Int -> Int
adr !f !t = rows * f + t

newHist :: IO History
newHist = V.replicate vsize 0

histw :: Int -> Int
histw = unsafeShiftL hv
    where hv = 1 `unsafeShiftL` (maxd + 1)
          maxd = 20

toHist :: History -> Square -> Square -> Int -> IO ()
toHist h f t = addHist h (adr f t) . histw

addHist :: History -> Int -> Int -> IO ()
addHist h !ad !p = do
    a <- V.unsafeRead h ad
    let !u = a - p	-- trick here: we subtract, so that the sort is big to small
        !v = if u < lowLimit then lowHalf else u
    V.unsafeWrite h ad v
    where lowLimit = -1000000000
          lowHalf  =  -500000000

valHist :: History -> Square -> Square -> IO Int
valHist !h !f !t = V.unsafeRead h $! adr f t
