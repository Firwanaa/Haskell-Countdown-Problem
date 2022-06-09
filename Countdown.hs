-- Countdown.hs
-- Programming in Haskell - Ch 9
--- The Countdown problem
module Countdown where

import Data.List
import System.IO

x = True

-- Arithmetic operators
data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

-- test if two positive integer numbers operations gives another positive natural number
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

-- apply function to perform the valid operations
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

-- Declare type of numeric expressions
data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n) = show n
  show (App o l r) = brak l ++ show o ++ brak r
    where
      brak (Val n) = show n
      brak e = "(" ++ show e ++ ")"

-- function returns the list of values in expression
values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

-- eval function: returns the over all value of an expressoin is positive natural number
-- > eval (App Add (Val 2) (Val 3))
-- > [5]
eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) =
  [ apply o x y | x <- eval l, y <- eval r, valid o x y]

-- Combinatorial functions
-- subs: gives all possible combinations
-- > subs [1,2,3]
--   [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x : xs) = yss ++ map (x :) yss
  where
    yss = subs xs

-- interleave: all possible ways to insert element into a list
-- > interleave 9 [1,2,3]
--   [[9,1,2,3],[1,9,2,3],[1,2,9,3],[1,2,3,9]
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y : ys) = (x : y : ys) : map (y :) (interleave x ys)

-- perms: returns all permutations of a list --
-- > perms [1,2,3]
--   [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x : xs) = concat (map (interleave x) (perms xs)) -- 🤯

-- function returns all choices
choices :: [a] -> [[a]]
choices = concatMap perms . subs -- concat . map perms . subs
-- Break down choices
-- > subs [1,2]
--   [[],[2],[1],[1,2]]
-- > map perms $ [[],[2],[1],[1,2]]
--   [[[]], [[2]], [[1]], [[1, 2], [2, 1]]]
-- > concat [[[]],[[2]],[[1]],[[1,2],[2,1]]]
--   [[],[2],[1],[1,2],[2,1]]

-- formalizing the problem
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = 
        elem (values e) (choices ns) && eval e == [n]
-- testing
-- breaking down (1 + 50) * (25 - 10)
left = App Add (Val 1) (Val 50)
right = App Sub (Val 25) (Val 10)
e = App Mul left right -- (1 + 50) * (25 - 10)
numlst::[Int]
numlst = [1,3,7,10,25,50]
targetnum = 765

-- > solution e numlst 765
--   True

-- lets break down "solution e numlst 765"
-- > valueslist values e
--   [1,50,25,10]
-- > choiceslist = choices numlst  
--   huge list
-- > elem valueslist choiceslist
--   True
-- > eval e 
--   [765]
-- > eval e == targetnum
--   True

-- Brute force solution