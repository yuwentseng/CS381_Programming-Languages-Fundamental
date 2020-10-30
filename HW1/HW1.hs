import HW1types
import Data.List


g :: Graph
g = [(1,2),(1,3),(2,3),(2,4),(3,4)]
h :: Graph
h = [(1,2),(1,3),(2,1),(3,2),(4,4)]
f = [Pt (4,4), Circle (5,5) 3, Rect (3,3) 7 2]

ins :: Eq a => a -> Bag a -> Bag a
ins x [] = [(x,1)]
ins x ((y,n):ms) | x==y      = (y,n+1):ms
                 | otherwise = (y,n):ins x ms

del :: Eq a => a -> Bag a -> Bag a
del x [] = []
del x ((y,n):ms) | x==y && n>1 = (y,n-1):ms
                 | x==y        = ms
                 | otherwise   = (y,n):del x ms

bag :: Eq a => [a] -> Bag a
bag = foldr ins []

subbag :: Eq a => Bag a -> Bag a -> Bool
subbag [] _ = True
subbag ((x,n):xs) ys = case lookup x ys of
                         Just m -> n<=m && subbag xs ys
                         Nothing -> False

isbag :: Eq a => Bag a -> Bag a -> Bag a
isbag [] _ = []
isbag ((x,n):xs) ys = case lookup x ys of
                         Just m -> (x,min n m):isbag xs ys
                         Nothing -> isbag xs ys

size :: Bag a -> Int
size = sum . map snd

xs,ys :: [Int]
xs = reverse [5,7,2,3,7,8,3,7]
ys = reverse [5,5,7,8,3,8,7]

lx = bag xs
ly = bag ys
lz = del 8 ly
la = del 5 lz
lb = del 3 la



nodes :: Graph -> [Node]
nodes [] = []
nodes (x:xs) = norm ( fst x : nodes xs ++ snd x : nodes xs )

suc :: Node -> Graph -> [Node]
suc x g = if x /= last (nodes g) then succ x : suc (succ x) g else []

detach :: Node -> Graph -> Graph
detach _ [] = []
detach f (x:xs) = if f == fst x || f == snd x then detach f xs else x: detach f xs   

cyc :: Int -> Graph
cyc x = zip [1..x] [2..x] ++[(x,1)]

width :: Shape -> Length
width sh = case sh of Pt (x,y) -> x - y
                      Circle (x,y) z -> z*2
                      Rect (x,y) z k -> z

bbox :: Shape -> BBox
bbox sh = case sh of  Pt (x,y) -> ((x,y),(x,y))
                      Circle (x,y) z -> ((x-z,y-z),(x+z,y+z))
                      Rect (x,y) z k -> ((x,y),(x+z,y+k))

minX :: Shape -> Number
minX sh = case sh of  Pt (x,y) -> min (min x y) (min x y)
                      Circle (x,y) z -> min (min ((fst (fst (bbox (Circle (x,y) z)))) ) ((snd (fst (bbox (Circle (x,y) z)))) )) (min ((fst (snd (bbox (Circle (x,y) z)))) ) ((snd (snd (bbox (Circle (x,y) z)))) ))
                      Rect (x,y) z k -> min (min ((fst (fst (bbox (Rect (x,y) z k)))) ) ((snd (fst (bbox (Rect (x,y) z k)))) )) (min ((fst (snd (bbox (Rect (x,y) z k)))) ) ((snd (snd (bbox (Rect (x,y) z k)))) ))

addPt :: Point -> Point -> Point
addPt (x,y) (q,w) = (x+q,y+w)

move :: Shape -> Point -> Shape
move (Pt (x,y)) (w,q) = Pt (addPt (x,y) (w,q))
move (Rect (x,y) z k) (w,q) = Rect (addPt (x,y) (w,q)) z k 
move (Circle (x,y) z) (w,q) = Circle (addPt (x,y) (w,q)) z  


moveToX :: Number -> Shape -> Shape
moveToX no (Pt pt)= Pt (no, snd pt)
moveToX no (Circle pt r)   = Circle (no + r, snd pt) r
moveToX no (Rect p r1 r2) = Rect (no, snd p) r1 r2


alignLeft :: Figure -> Figure
alignLeft f = map (moveToX (head (sort (map minX f)))) f




shapearea :: Shape -> Int
shapearea sh = case sh of Pt (x,y) -> 0
                          Circle (x,y) z -> round( pi * (fromInteger ((2^2) :: Integer) :: Double ))
                          Rect (x,y) z k -> z * k

inside :: Shape -> Shape -> Bool
inside sh1 sh2
       | shapearea sh1 == shapearea sh2 && shapearea sh2 == 0 && (fst (bbox sh1) == fst (bbox sh2) && snd (bbox sh1) == snd (bbox sh2)) = True
       | (fst (bbox sh1) >= fst (bbox sh2) && snd (bbox sh1) <= snd (bbox sh2)  && shapearea sh1 <= shapearea sh2) = True 
       | otherwise = False

        



