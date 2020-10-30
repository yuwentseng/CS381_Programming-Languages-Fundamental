module HW1types where

import Data.List (nub,sort)

type Bag a = [(a,Int)]
type Node  = Int
type Edge  = (Node,Node)
type Graph = [Edge]
type Path  = [Node]

norm :: Ord a => [a] -> [a]
norm = sort . nub

type Number = Int
type Point = (Number,Number)
type Length = Number
data Shape = Pt Point
           | Circle Point Length
           | Rect Point Length Length
           deriving Show
type Figure = [Shape]
type BBox = (Point,Point)
type Area = Double