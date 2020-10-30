
module HW4 where

import Data.Maybe

-- Exercise 1
-- (a)
type Prog = [Cmd]
type Stack = [Int]
type D = Stack -> Maybe Stack
type Rank = Int
type CmdRank = (Int,Int)

data Cmd    = LD Int
            | ADD
            | MULT
            | DUP
            | INC
            | SWAP
            | POP Int
            deriving Show

rankC :: Cmd -> CmdRank
rankC (LD i)        = (0,1)
rankC (ADD)         = (2,1)
rankC (MULT)        = (2,1)
rankC (DUP)         = (1,2)
rankC (INC)         = (1,1)
rankC (SWAP)        = (2,2)
rankC (POP i)       = (i,0)

rank :: Prog -> Rank -> Maybe Rank
rank [] r           = Just r
rank (x:xs) r       | r >= fst rc && isJust rcs = Just $ fromJust $ rcs
                    | otherwise                 = Nothing
                    where   rc = rankC x
                            rcs = rank xs (r - fst rc + snd rc)

rankP :: Prog -> Maybe Rank
rankP p             = rank p 0

semCmd :: Cmd -> D
semCmd (LD i) se     = Just (i:se)
semCmd ADD (a:b:se)  = Just $ (a+b):se
semCmd MULT (a:b:se) = Just $ (a*b):se
semCmd DUP (a:se)    = Just $ a:a:se
semCmd INC (a:se)    = Just $ a+1:se
semCmd SWAP (a:b:se) = Just $ b:a:se
semCmd (POP 0) se    = Just $ se
semCmd (POP i) (_:se)= semCmd (POP (i-1)) se
semCmd _ _          = Nothing

sem :: Prog -> D
sem [] se            = Just se
sem (a:b) se         = case semCmd a se of
                        Just stack  -> sem b stack
                        Nothing     -> Nothing
-- (b)

initStack = []

semStatTC :: Prog -> Maybe Stack
semStatTC s         = case rankP s of
                        Just r      -> sem s initStack
                        otherwise   -> Nothing

-- The type of the sem function can be simplified if it is called from
-- semStatTC, because the type checking that is performed first ensures that a
-- Stack will be returned, rather than a Maybe Stack.  Therefore, its type may
-- be Prog -> Stack -> Stack.  Further, this simplifies its implementation as
-- well because semCmd will also return a Stack rather than a Maybe Stack, so
-- testing for the Nothing case is not necessary.

-- These tests can be run easily to verify the correctness of the solution.

-- Tests the semantics of all operations
testProg1 = [LD 3, DUP, MULT, LD 2, INC, LD 2, SWAP, ADD, LD 1, LD 1, POP 2]
testResult1 = semStatTC testProg1

-- Returns Nothing due to type error (popping two elements from a stack of one)
testProg2 = [LD 3, POP 2]
testResult2 = semStatTC testProg2

-- Exercise 2
data Shape = X 
           | TD Shape Shape
           | LR Shape Shape
           deriving Show

type Pixel = (Int,Int)
type Image = [Pixel]

sem2 :: Shape -> Image 
sem2 X           = [(1,1)]
sem2 (LR s1 s2) = d1 ++ [(x+maxx d1,y) | (x,y) <- sem2 s2] 
                 where d1 = sem2 s1
sem2 (TD s1 s2) = d2 ++ [(x,y+maxy d2) | (x,y) <- sem2 s1] 
                 where d2 = sem2 s2

maxx :: [Pixel] -> Int
maxx = maximum . map fst

maxy :: [Pixel] -> Int
maxy = maximum . map snd

-- (a)
type BBox = (Int, Int)

bbox :: Shape -> BBox
bbox s          = (maxx i,maxy i)
                where i = sem2 s

-- (b)
rect :: Shape -> Maybe BBox
rect s          | length i == (maxx i)*(maxy i) = Just $ bbox s
                | otherwise                     = Nothing
                where i = sem2 s

testShape1 = TD (LR X X) (LR X X) 
testBbox1 = bbox testShape1 
testRect1 = rect testShape1 

testShape2 = TD (LR X X) (LR X (TD X X)) 
testBbox2 = bbox testShape2 
testRect2 = rect testShape2 

-- Exercise 3
-- (a)
f x y = if null x then [y] else x
g x y = if not (null x) then [] else [y]
-- 1.   f :: [t] -> t -> [t]
--      g :: [a] -> a1 -> [a1]
-- 2. In f, x must be a list to be used in the null function.  Additionally, x and y need to contain and be the same type, respectively, since a list of that type derived from either of them can be returned.
--      In g, x just needs to be a list (since it is used in the null function).  x does not need to be the same type as y because a list of type y will be returned regardless of what type x is.
-- 3.   g has a more general type than f because x and y do not need to have types that match up, whereas f requires that x and y contain and be the same type, respectively.
-- 4. In f, x must be a list to be used in the null function.  Additionally, x and y need to contain and be the same type, respectively, since a list of that type derived from either of them can be returned.
--      In g, x just needs to be a list (since it is used in the null function).  x does not need to be the same type as y because a list of type y will be returned regardless of what type x is.
-- (b)
--h :: [b] -> [(a, b)] -> [b]
h x y = if null x then [head x] else [snd (head y)]
-- This returns a type of h :: [t] -> [(a, t)] -> [t], which is the same

-- (c)
--k :: (a -> b) -> ((a -> b) -> a) -> b
k f g = f (g f)
-- This returns a type of k :: (t1 -> t2) -> ((t1 -> t2) -> t1) -> t2, which is functionally equivalent.

-- (d)
-- Can not define a function of type a -> b because there is not possible to imply "a variable of any type except one other particular type"; it is only possible to imply matching types or specific types.
