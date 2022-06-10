module Main where

import Data.List
import System.IO

data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

-- valid :: Op -> Int -> Int -> Bool
-- valid Add _ _ = True
-- valid Sub x y = x > y
-- valid Mul _ _ = True
-- valid Div x y = x `mod` y ==p 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n) = show n
  show (App o l r) = brak l ++ show o ++ brak r
    where
      brak (Val n) = show n
      brak e = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) =
  [ apply o x y | x <- eval l, y <- eval r, valid o x y]
-- > eval (App Add (Val 2) (Val 3))
-- > [5]

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x : xs) = yss ++ map (x :) yss
  where
    yss = subs xs
-- subs: gives all possible combinations
-- > subs [1,2,3]
--   [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y : ys) = (x : y : ys) : map (y :) (interleave x ys)
-- interleave: all possible ways to insert element into a list
-- > interleave 9 [1,2,3]
--   [[9,1,2,3],[1,9,2,3],[1,2,9,3],[1,2,3,9]

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x : xs) = concat (map (interleave x) (perms xs)) -- 🤯
-- perms: returns all permutations of a list --
-- > perms [1,2,3]
--   [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]

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

split :: [a] ->  [([a], [a])]
split [] = []
split [_] = []
split (x:xs) = ([x], xs) : [(x:ls,rs) | (ls,rs) <- split xs]
-- > split [1,2,3,4]
-- [([1],[2,3,4]),([1,2],[3,4]),([1,2,3],[4])]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) <- split ns,
              l <- exprs ls,
              r <- exprs rs,
              e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

type Result = (Expr, Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n,n) | n > 0]
results ns = [res | (ls,rs) <- split ns,
                     lx     <- results ls,
                     ry     <- results rs,
                     res    <- combine' lx ry]

combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid o x y]


solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e,m) <- results ns', m == n]

-- x + y == y + x
-- x * y == y * x
-- x * 1 == x
-- 1 * y == y
-- x / 1 == x

-- modify valid function
valid :: Op -> Int -> Int -> Bool
valid Add x y = x <=y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y/= 1 && x <= y
valid Div x y = y /= 1 && x `mod` y == 0

main :: IO ()
-- main = print (solutions [1,3,7,10,25,50] 765) -- will take longer time
main = print (solutions' [1,3,7,10,25,50] 765) -- fast
