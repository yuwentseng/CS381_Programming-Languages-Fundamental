

{----------------------- Exercise 1 -------------------------}

type Prog = [Cmd]

data Cmd    = LD Int
            | ADD
            | MULT
            | DUP
            | DEF String Prog
            | CALL String
            deriving Show
type Stack = [Int]

type D = Maybe Stack -> Maybe Stack


semCmd :: Cmd -> D
semCmd (LD a) ys = case ys of Just ys         -> Just ([a] ++ ys)
                              _               -> Nothing
semCmd (ADD)  ys = case ys of Just (y1:y2:ys) -> Just ([y1+y2] ++ ys)
                              _               -> Nothing
semCmd (DUP) ys  = case ys of Just (y1:ys)    -> Just ([y1,y1] ++ ys)
                              _               -> Nothing
semCmd (MULT) ys = case ys of Just (y1:y2:ys) -> Just ([y1*y2] ++ ys)
                              _               -> Nothing

sem :: Prog -> D
sem [] a = a
sem (y:ys) a = sem ys (semCmd y a)

eval :: Prog -> Maybe Stack
eval p = sem p (Just [])

--Hint. Test your functions with the programs [LD 3,DUP,ADD,DUP,MULT] and [LD 3,ADD] and the empty stack [] as inputs.
q1t = [LD 3, DUP, ADD, DUP, MULT]
q2t = [LD 3, ADD]
{----------------------- Exercise 2 -------------------------}

-- (a) Done; see Cmd data type above.
-- (b)
type State = (Macros,Stack)
type Macros = [(String,Prog)]
type D2 = State -> Maybe State

-- (c)
semCmd2 :: Cmd -> D2
semCmd2 (LD i) (m,s)    = Just (m,i:s)
semCmd2 ADD (m,a:b:s)   = Just (m,(a+b):s)
semCmd2 MULT (m,a:b:s)  = Just (m,(a*b):s)
semCmd2 DUP (m,a:s)     = Just (m,a:a:s)
semCmd2 (DEF n p) (m,s) = Just ((n,p):m,s)
semCmd2 (CALL n) (m,s)  = case lookup n m of
                            Just p      -> sem2 p (m,s)
                            otherwise   -> sem2 [] (m,s)
semCmd2 _ _             = Nothing

sem2 :: Prog -> D2
sem2 [] (m,s)           = Just (m,s)
sem2 (a:p) (m,s)        = case semCmd2 a (m,s) of
                            Just state  -> sem2 p state
                            otherwise   -> Nothing

begin2 = ([],[])
q21t = [DEF "SQR" [DUP, MULT], LD 3, CALL "SQR", LD 2, DUP, ADD]
q22t = sem2 q21t begin2
{----------------------- Exercise 3 -------------------------}

data Cmdthd = Pen Mode
          | MoveTo Int Int
          | Seq Cmdthd Cmdthd
          deriving Show

data Mode = Up | Down
          deriving (Show, Eq)

type State3 = (Mode,Int,Int)
type Line = (Int,Int,Int,Int)
type Lines = [Line]

semS :: Cmdthd -> State3 -> (State3,Lines)
semS (Pen s1)       s@(s2, a, b) | s1 /= s2             = ((s1, a, b), [])
                                 | otherwise            = (s, [])
semS (MoveTo a1 b1) (m, a2, b2)  | m == Up              = (ns, [])
                                 | a1 /= a2 && b1 /= b2 = (ns, [(a2, b2, a1, b1)])
                                 | otherwise            = (ns, [])
                                 where ns = (m, a1, b1)
semS (Seq a b) s = (fst s2, snd s1 ++ snd s2)
                 where 
                    s1 = semS a s
                    s2 = semS b (fst s1)

-- front
sinit = (Up, 0, 0)

sem' :: Cmdthd -> Lines
sem' a = snd (semS a sinit)

q3t11 = Pen Down `Seq` MoveTo 1 1
q3t12 = sem' q3t11 -- [(0, 0, 1, 1)]
q3t21 = Pen Down `Seq` MoveTo 1 1 `Seq` MoveTo 3 5
q3t22 = sem' q3t21 -- [(0, 0, 1, 1), (1, 1, 3, 5)]