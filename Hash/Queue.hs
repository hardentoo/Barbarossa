{-# LANGUAGE BangPatterns #-}

module Hash.Queue (
    Queue, newQueue, addQueue, listQueue
) where

import Data.Foldable (toList)
import qualified Data.Sequence as S
import Data.Sequence (Seq, (<|), (|>), (><), ViewL((:<)))

-- A finite queue of values ordered by a key
-- Only the highest keys seen are kept
data Queue a = Queue {
                   maxl, mink, maxk :: !Int,
                   vals :: Seq (Int, a)
               }

newQueue :: Int -> Queue a
newQueue mx = Queue { maxl = mx, mink = maxBound, maxk = minBound, vals = S.empty }

listQueue :: Queue a -> [(Int, a)]
listQueue = toList . vals

addQueue :: a -> Int -> Queue a -> Queue a
addQueue a k q
    | S.null (vals q)            = q { mink = k, maxk = k, vals = S.singleton (k, a) }	-- first element
    | S.length (vals q) < maxl q = add a k q	-- true add
    | otherwise                  = adr a k q	-- add & remove

{-# INLINE add #-}
add :: a -> Int -> Queue a -> Queue a
add a k q
    | k <= mink q = q { mink = k, vals = (k, a) <| vals q }
    | k >= maxk q = q { maxk = k, vals = vals q |> (k, a) }
    | otherwise   = let (s1, s2) = S.spanl ((< k) . fst) (vals q)
                    in q { vals = s1 >< ((k, a) <| s2) }

{-# INLINE adr #-}
adr :: a -> Int -> Queue a -> Queue a
adr a k q
    | k <= mink q                = q	-- too small
    | k >= maxk q = let s1 = vals q |> (k, a)
                        _ :< s2 = S.viewl s1
                    in q { maxk = k, vals = s2 }
    | otherwise   = let (s1, s2) = S.spanl ((< k) . fst) (vals q)
                        _ :< s3 = S.viewl $ s1 >< ((k, a) <| s2)
                    in q { vals = s3 }
