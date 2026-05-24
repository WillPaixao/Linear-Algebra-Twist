{-# LANGUAGE ConstrainedClassMethods, InstanceSigs #-} 

import Data.List (intercalate, transpose)

-- Defining the interface of operations

class Semiring a where
  (⊕), (⊗) :: Semiring a => a -> a -> a
  zero, one :: Semiring a => a
  -- TODO: include closure

-- Instantiating integers and rationals as semirings

instance Semiring Int where
  (⊕) :: Int -> Int -> Int
  (⊕) = (+)

  (⊗) :: Int -> Int -> Int
  (⊗) = (*)

  zero :: Int
  zero = 0
  
  one :: Int
  one = 1

instance Semiring Double where
  (⊕) :: Double -> Double -> Double
  (⊕) = (+)

  (⊗) :: Double -> Double -> Double
  (⊗) = (*)

  zero :: Double
  zero = 0.0

  one :: Double
  one = 1.0

-- Defining usual linear algebra constructs

--- Matrices and their operations

type Matrix a = [[a]]

zeroMatrix :: Semiring a => Int -> Matrix a
zeroMatrix d = replicate d (replicate d zero)

idMatrix :: Semiring a => Int -> Matrix a
idMatrix d = places one (replicate (d-1) zero)

sumMatrixMatrix :: Semiring a => Matrix a -> Matrix a -> Matrix a
sumMatrixMatrix x y = [zipWith (⊕) rx ry | (rx,ry) <- zip x y]

multMatrixMatrix :: Semiring a => Matrix a -> Matrix a -> Matrix a
multMatrixMatrix x y = [[foldr (⊕) zero (zipWith (⊗) rx cy) | cy <- transpose y] | rx <- x]

powersMatrix :: Semiring a => Matrix a -> [Matrix a]
powersMatrix m = iterate (multMatrixMatrix m) (idMatrix (length m))

--- Vectors and their operations 

type Vector a = [a]

vec2Matrix :: Vector a -> Matrix a
vec2Matrix v = [v]

matrix2Vec :: Matrix a -> Vector a
matrix2Vec m | length m == 1           = head m
             | all ((== 1) . length) m = head (transpose m)
             | otherwise               = error "Cannot convert given matrix to a vector!"

sumVecVec :: Semiring a => Vector a -> Vector a -> Vector a
sumVecVec u v = matrix2Vec (vec2Matrix u `sumMatrixMatrix` vec2Matrix v)

multMatrixVec :: Semiring a => Matrix a -> Vector a -> Vector a
multMatrixVec m v = matrix2Vec (m `multMatrixMatrix` transpose (vec2Matrix v))

multVecMatrix :: Semiring a => Vector a -> Matrix a -> Vector a
multVecMatrix v m = matrix2Vec (vec2Matrix v `multMatrixMatrix` m)

---- Inner product
(@) :: Semiring a => Vector a -> Vector a -> a
u @ v =
  if length u == length v then
    head (head (vec2Matrix u `multMatrixMatrix` transpose (vec2Matrix v)))
  else
    error "Cannot take inner product from vectors of different dimensions!"

-- Defining existence of paths

instance Semiring Bool where
  (⊕) :: Bool -> Bool -> Bool
  (⊕) = (||)

  (⊗) :: Bool -> Bool -> Bool
  (⊗) = (&&)

  zero :: Bool
  zero = False

  one :: Bool
  one = True

--- Example matrices
m2 = 
  [
    [False, True , True , False, False],
    [False, False, True , False, False],
    [False, False, False, False, False],
    [False, False, False, True , True ],
    [False, False, False, True , False]
  ]

-- Defining min-plus case for adjacency matrices

data Edge = W Int | Inf
  deriving (Eq, Ord, Show)

instance Semiring Edge where
  (⊕) :: Edge -> Edge -> Edge
  W x ⊕ W y = W (min x y)
  Inf ⊕ e   = e
  e   ⊕ Inf = e

  (⊗) :: Edge -> Edge -> Edge
  W x ⊗ W y = W (x + y)
  _   ⊗ _   = Inf

  zero :: Edge
  zero = Inf

  one :: Edge
  one = W 0

shortestPaths :: Semiring a => Matrix a -> Matrix a
shortestPaths m = foldr sumMatrixMatrix (zeroMatrix n) powers
  where
    n = length m
    powers = take (n+1) (powersMatrix m)

--- Example matrices
m1 :: Matrix Edge
m1 = 
  [
    [Inf, W 1, W 1, Inf, Inf],
    [Inf, W 1, Inf, Inf, W 2],
    [Inf, W 1, Inf, W 2, Inf],
    [W 1, W 1, Inf, Inf, W 1],
    [Inf, Inf, Inf, W 1, Inf]
  ]

-- Defining an analogous to the min-plus case, but taking paths into account

type Vertex = Int
data Path = Path [Vertex] | None
  deriving (Eq, Ord, Show)

instance Semiring Path where
  (⊕) :: Path -> Path -> Path
  None    ⊕ q       = q
  p       ⊕ None    = p
  Path ps ⊕ Path qs =
    if length ps < length qs then
      Path ps
    else
      Path qs

  (⊗) :: Path -> Path -> Path
  None    ⊗ _       = None
  _       ⊗ None    = None
  Path ps ⊗ Path qs = Path (ps ++ qs)

  zero :: Path
  zero = None

  one :: Path
  one = Path []

--- Example matrices
m1' :: Matrix Path
m1' = 
  [
    [None, Path [1], Path [1], None, None],
    [None, Path [], None, None, Path [2]],
    [None, Path [3], None, Path [3], None],
    [Path [4], Path [4], None, None, Path [4]],
    [None, None, None, Path [5], None]
  ]

-- TODO: find more uses of semirings!

instance Semiring [String] where
  (⊕) :: [String] -> [String] -> [String]
  (⊕) = (++)

  (⊗) :: [String] -> [String] -> [String]
  ss1 ⊗ ss2 = (++) <$> ss1 <*> ss2

  zero :: [String]
  zero = []

  one :: [String]
  one = [""]

partialClosures :: Semiring a => Matrix a -> [Matrix a]
partialClosures m = 
  scanl sumMatrixMatrix (zeroMatrix (length m)) (powersMatrix m)

acceptedLanguage :: Matrix [String] -> Int -> Int -> Int -> [String]
acceptedLanguage a n i f = partialClosures a !! (n+1) !! i !! f

a1 =
  [
    [[   ], ["a","b"], ["c"]],
    [[   ], ["c"], ["a"]],
    [["b"], [   ], [   ]]
  ]

-- Auxiliar functions

places :: a -> [a] -> [[a]]
places x [] = [[x]]
places x (y:ys) = (x:y:ys) : map (y:) (places x ys)

pad :: Int -> String -> String
pad n cs = cs ++ replicate (n - length cs) ' '

putMatrix :: Show a => Matrix a -> IO ()
putMatrix [] = return ()
putMatrix (r:rs) = do
  let es = concat (r:rs)
  let n = maximum (map (length . show) es)
  putStrLn (intercalate "\t" (map (pad n . show) r))
  putMatrix rs

main :: IO ()
main = print $ acceptedLanguage a1 10 0 2