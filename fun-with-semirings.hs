import Data.List

-- Declaring semiring class

infixl 9 ⊗
infixl 8 ⊕
class ClosedSemiring a where
  (⊕), (⊗) :: a -> a -> a
  zero, one  :: a
  closure    :: a -> a

-- Declaring matrices and its methods

data Matrix a = Scalar a | Matrix [[a]]
  deriving Show

type BlockMatrix a = (Matrix a, Matrix a,
                      Matrix a, Matrix a)

mjoin :: BlockMatrix a -> Matrix a
mjoin (Matrix a, Matrix b,
       Matrix c, Matrix d) =
         Matrix ((a `hcat` b) ++ (c `hcat` d))
           where hcat = zipWith (++)

msplit :: Matrix a -> BlockMatrix a
msplit (Matrix (row:rows)) =
  (Matrix [[first]], Matrix [top],
   Matrix left,      Matrix rest)
  where
    (first:top) = row
    (left, rest) = unzip (map (\(x:xs)-> ([x],xs))
                         rows)

instance ClosedSemiring a => ClosedSemiring (Matrix a) where
  (⊕) :: Matrix a -> Matrix a -> Matrix a
  (Scalar x) ⊕ (Scalar y) = Scalar (x ⊕ y)
  (Matrix m) ⊕ (Matrix n) = Matrix (zipWith (zipWith (⊕)) m n)
  (Scalar x) ⊕ (Matrix m) = Matrix m ⊕ Scalar x
  (Matrix [[z]]) ⊕ (Scalar x) = Matrix [[z ⊕ x]]
  m ⊕ x = mjoin (ma ⊕ x, mb,
                  mc,      md ⊕ x)
            where
              (ma, mb, mc, md) = msplit m

  (⊗) :: Matrix a -> Matrix a -> Matrix a
  (Scalar x) ⊗ (Scalar y) = Scalar (x ⊗ y)
  (Matrix m) ⊗ (Matrix n) = 
    Matrix [[foldr (⊕) zero (zipWith (⊗) rm cn) | cn <- transpose n] | rm <- m]
  (Scalar x) ⊗ (Matrix m) = Matrix (map (map (x ⊗)) m)
  (Matrix m) ⊗ (Scalar x) = Matrix (map (map (⊗ x)) m)

  zero :: Matrix a
  zero = Scalar zero

  one :: Matrix a
  one = Scalar one

  closure :: Matrix a -> Matrix a
  closure (Matrix [[x]]) = Matrix [[closure x]]
  closure m = mjoin
    (mbS ⊕ mbS ⊗ mc ⊗ deltaS ⊗ md ⊗ mbS, mbS ⊗ mc ⊗ deltaS,
     deltaS ⊗ md ⊗ mbS,                     deltaS)
      where
        (mb, mc, md, me) = msplit m
        mbS = closure mb
        delta = me ⊕ md ⊗ mbS ⊗ mc
        deltaS = closure delta

-- Instantiating some semirings

--- Booleans

instance ClosedSemiring Bool where
  (⊕) :: Bool -> Bool -> Bool
  (⊕) = (||)

  (⊗) :: Bool -> Bool -> Bool
  (⊗) = (&&)

  zero :: Bool
  zero = False

  one :: Bool
  one = True

  closure :: Bool -> Bool
  closure b = True

---- Example matrices

mb1 :: Matrix Bool
mb1 = Matrix [[False, False, True , False, True ],
              [True , False, False, True , False],
              [False, True , False, False, False],
              [False, False, False, False, True ],
              [False, False, False, True , False]]

--- Tropical Semiring

data Tropical = ValT Int | InfT
  deriving Show

instance ClosedSemiring Tropical where
  (⊕) :: Tropical -> Tropical -> Tropical
  InfT ⊕ y = y
  x ⊕ InfT = x
  (ValT x) ⊕ (ValT y) = ValT (min x y)

  (⊗) :: Tropical -> Tropical -> Tropical
  (ValT x) ⊗ (ValT y) = ValT (x + y)
  _ ⊗ _ = InfT

  zero :: Tropical
  zero = InfT

  one :: Tropical
  one = ValT 0

  closure :: Tropical -> Tropical
  closure _ = ValT 0

---- Example matrices

mt1 :: Matrix Tropical
mt1 = Matrix [[InfT, ValT 10, InfT, InfT, InfT],
              [InfT, InfT, ValT 5, ValT 15, InfT],
              [ValT 20, InfT, InfT, ValT 5, InfT],
              [InfT, InfT, InfT, InfT, ValT 3],
              [InfT, InfT, InfT, ValT 19, InfT]]

--- Extended Reals

data ExtendedDouble = ValD Double | InfD
  deriving Show

instance ClosedSemiring ExtendedDouble where
  (⊕) :: ExtendedDouble -> ExtendedDouble -> ExtendedDouble
  (ValD x) ⊕ (ValD y) = ValD (x + y)
  _ ⊕ _ = InfD

  (⊗) :: ExtendedDouble -> ExtendedDouble -> ExtendedDouble
  (ValD x) ⊗ (ValD y) = ValD (x * y)
  _ ⊗ _ = InfD

  zero :: ExtendedDouble
  zero = ValD 0

  one :: ExtendedDouble
  one = ValD 1

  closure :: ExtendedDouble -> ExtendedDouble
  closure (ValD 1) = InfD
  closure (ValD x) = ValD (1 / (1 - x))
  closure InfD = InfD

mapMatrix :: (a -> b) -> [[a]] -> [[b]]
mapMatrix f = map (map f)

extendDoubleMatrix :: [[Double]] -> [[ExtendedDouble]]
extendDoubleMatrix = mapMatrix ValD

inverseMatrix :: Matrix ExtendedDouble -> Matrix ExtendedDouble
inverseMatrix m = closure (one ⊕ Scalar (ValD (-1)) ⊗ m)

---- Example matrices

med1 :: Matrix ExtendedDouble
med1 = Matrix (extendDoubleMatrix [[1, 0, 0],
                                   [0, 1, 0],
                                   [0, 0, 1]])

med2 :: Matrix ExtendedDouble
med2 = Matrix (extendDoubleMatrix [[1, 2, 3],
                                   [0, 5, 4],
                                   [0, 0, 6]])

med3 :: Matrix ExtendedDouble
med3 = Matrix (extendDoubleMatrix [[1 ,2 ,3 ],
                                   [20,10,30],
                                   [13,12,11]])

med4 :: Matrix ExtendedDouble
med4 = Matrix (extendDoubleMatrix [[1,4,7],
                                   [2,5,8],
                                   [3,6,9]])

med5 :: Matrix ExtendedDouble
med5 = Matrix (extendDoubleMatrix [[2,0,0],
                                   [0,5,0],
                                   [0,0,1]])

-- ???
med6 :: Matrix ExtendedDouble
med6 = Matrix (extendDoubleMatrix [[sqrt 2 / 2, - (sqrt 2 / 2), 0],
                                   [0         , 0             , 1],
                                   [sqrt 2 / 2, sqrt 2 / 2    , 0]])

--- Regular Expressions

data FreeSemiring gen =
    Zero
  | One
  | Gen gen
  | Closure (FreeSemiring gen)
  | (FreeSemiring gen) :⊕ (FreeSemiring gen)
  | (FreeSemiring gen) :⊗ (FreeSemiring gen)

instance Show (FreeSemiring Char) where
  show :: FreeSemiring Char -> String
  show Zero = "∅"
  show One = "ε"
  show (Gen c) = [c]
  show (Closure fs) = enclose (show fs) ++ "*"
  show (x :⊕ y) = enclose (show x ++ "+" ++ show y)
  show (x :⊗ y) = show x ++ show y

instance ClosedSemiring (FreeSemiring gen) where
  (⊕) :: FreeSemiring gen -> FreeSemiring gen -> FreeSemiring gen
  Zero ⊕ x = x
  x ⊕ Zero = x
  x ⊕ y = x :⊕ y

  (⊗) :: FreeSemiring gen -> FreeSemiring gen -> FreeSemiring gen
  Zero ⊗ x = Zero
  x ⊗ Zero = Zero
  One ⊗ x = x
  x ⊗ One = x
  x ⊗ y = x :⊗ y

  zero :: FreeSemiring gen
  zero = Zero
  
  one :: FreeSemiring gen
  one = One

  closure :: FreeSemiring gen -> FreeSemiring gen
  closure Zero = One
  closure x = Closure x

---- Example matrices

mrx1 :: Matrix (FreeSemiring Char)
mrx1 = Matrix [[Zero   , Gen 'x', Zero   ],
               [Gen 'y', Zero   , Gen 'z'],
               [Zero   , Zero   , Zero   ]]

mrx2 :: Matrix (FreeSemiring Char)
mrx2 = Matrix [[Zero   , Gen 'a'],
               [Gen 'b', Gen 'c']]

-- UI utilities

enclose :: String -> String
enclose cs = "(" ++ cs ++ ")"

pad :: Int -> String -> String
pad n cs = cs ++ replicate (n - length cs) ' '

columnWidths :: Show a => [[a]] -> [Int]
columnWidths m = [maximum [length (show x) | x <- col] | col <- transpose m]

putMatrix :: Show a => Matrix a -> IO ()
putMatrix (Matrix m) = putRow m (columnWidths m)
  where
    putRow [] _ = return ()
    putRow (r:rs) ws = do
      putStrLn (unwords (zipWith (\w x -> pad w (show x)) ws r))
      putRow rs ws