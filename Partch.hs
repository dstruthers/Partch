module Partch where
import Data.Array
import Data.List (nub, sort)
import Data.Ratio

data Interval = Interval { fromInterval :: Ratio Int }
              deriving (Eq, Ord)

instance Num Interval where
  (Interval r1) + (Interval r2) = simplify $ Interval $ r1 + r2
  (Interval r1) - (Interval r2) = simplify $ Interval $ r1 - r2
  (Interval r1) * (Interval r2) = simplify $ Interval $ r1 * r2
  negate (Interval r) = simplify $ Interval $ negate r
  abs (Interval r) = simplify $ Interval $ abs r
  signum (Interval r) = case compare r 0 of
    LT -> -1
    EQ -> 0
    GT -> 1
  fromInteger n = Interval $ fromInteger n
  
instance Show Interval where
  show (Interval r) = show (numerator r) ++ ":" ++ show (denominator r)

-- Some convenient interval definitions
unison   = Interval (1%1)
major2   = Interval (9%8)
major3   = Interval (5%4)
perfect4 = Interval (4%3)
perfect5 = Interval (3%2)
octave   = Interval (2%1)

octaveUp (Interval r) = Interval $ (2 * numerator r) % denominator r
octaveDown (Interval r) = Interval $ numerator r % (denominator r * 2)

isSimple :: Interval -> Bool
isSimple (Interval r) = r >= 1 && r <= 2

simplify :: Interval -> Interval
simplify i = if isSimple i
             then i
             else simplify $ case compare i unison of
               LT -> octaveUp i
               GT -> octaveDown i

inverse :: Interval -> Interval
inverse = simplify . Interval . recip . fromInterval

otone = simplify . Interval . (%1)
utone = simplify . Interval . (1%)

otones n = map otone . filter odd $ [1..n]
utones n = map utone . filter odd $ [1..n]

diamondList :: Int -> [Interval]
diamondList n = nub $ sort [x * y | x <- otones n, y <- utones n]

diamondArray :: Int -> Array (Int, Int) Interval
diamondArray n = array ((1,1), (n,n)) [((x,y), otone x * utone y) | x <- [1..n], y <- [1..n]]