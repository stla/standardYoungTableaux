{-# LANGUAGE BangPatterns #-}
module Enumeration
  ( ballotSequences
  , standardYoungTableaux
  )
where
import           Data.Sequence                  ( Seq
                                                , (<|)
                                                , (|>)
                                                , (><)
                                                , adjust'
                                                , update
                                                , index
                                                )
import qualified Data.Sequence                 as S

f1 :: Int -> Seq Int -> (Int, Seq Int)
f1 n a = go (-1) lambda0 1 False
 where
  lambda0 = 1 <| S.replicate (n - 1) 0
  go !isave !lambda i !descent
    | i == n || descent
    = (isave, lambda)
    | otherwise
    = let a_i = a `index` i
      in  let lmbd = adjust' (1 +) a_i lambda
          in  let dsct = a_i < a `index` (i - 1)
              in  let j = if dsct then i else isave in go j lmbd (i + 1) dsct

f2 :: Int -> Seq Int -> Seq Int -> Int -> (Seq Int, Seq Int, Int)
f2 n a0 lambda0 isave = go (n - 1) a0 lambda0 it0
 where
  it0 = lambda0 `index` ((a0 `index` isave) + 1)
  go i !a !lambda !it
    | i == (-1)
    = (a, lambda, it)
    | lambda `index` i == it
    = (update isave i a, adjust' (subtract 1) i lambda, isave)
    | otherwise
    = go (i - 1) a lambda it

f3 :: Int -> Seq Int -> Seq Int -> Int -> Seq Int
f3 n a0 lambda0 it = go 0 0 a0 lambda0
 where
  go k ir !a !lambda
    | n - 1 < ir = a
    | lambda `index` ir /= 0 = go (k + 1)
                                  (ir + 1)
                                  (update k ir a)
                                  (adjust' (subtract 1) ir lambda)
    | it < k + 1 = a
    | otherwise = go k 0 a lambda

{- descent :: Seq Int -> Bool
descent a = go 0
 where
  n = S.length a
  go i = i < n - 1 && (a `index` (i + 1) < a `index` i || go (i + 1)) 
-}

ballot :: Int -> Seq Int -> Seq Int -> Bool -> (Seq Int, Bool)
ballot n a0 lambda0 more =
  let l = S.length lambda0
  in  let (isave, lambda1) = f1 n a0
      in  let (a1, lambda2, it0) = f2 n a0 lambda1 isave
          in  let (it, a3, lambda3) = if more
                    then (it0, a1, lambda2)
                    else (n, a0, lambda0 >< S.replicate (n - l) 0)
              in  if more && isave == -1
                    then (a1, False)
                    else
                      let a = f3 n a3 lambda3 it
                      in  if n == 1 then (a, True) else (a, descent a)
 where
  descent b = go 0
    where go i = i < n - 1 && (b `index` (i + 1) < b `index` i || go (i + 1))

ballotSequences :: [Int] -> [Seq Int]
ballotSequences lambda0 =
  let a0 = S.replicate n 0
  in  let (a, more) = ballot n a0 lambda False in go [a] more
 where
  n = sum lambda0
  lambda = S.fromList lambda0
  go !list !mr = if mr
    then let (a', mr') = ballot n (head list) lambda True in go (a' : list) mr'
    else list

ballot2syt :: Seq Int -> Seq (Seq Int)
ballot2syt a = go voidsyt 0
 where
  voidsyt = S.replicate (maximum a + 1) (S.empty :: Seq Int)
  n       = S.length a
  go !syt i
    | i == n    = syt
    | otherwise = go (update a_i ((syt `index` a_i) |> (i + 1)) syt) (i + 1)
    where a_i = a `index` i

standardYoungTableaux :: [Int] -> [Seq (Seq Int)]
standardYoungTableaux lambda = map tosyt (ballotSequences lambda)
 where
  tosyt a = go voidsyt 0
   where
    voidsyt = S.replicate (maximum a + 1) (S.empty :: Seq Int)
    n       = sum lambda
    go !syt i
      | i == n    = syt
      | otherwise = go (update a_i ((syt `index` a_i) |> (i + 1)) syt) (i + 1)
      where a_i = a `index` i

