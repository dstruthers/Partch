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
  signum (Interval r) = Interval $ signum r
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
isSimple (Interval r) = r >= 1 && r < 2

simplify :: Interval -> Interval
simplify i = if isSimple i
             then i
             else simplify $ if i < unison then octaveUp i else octaveDown i

inverse :: Interval -> Interval
inverse = simplify . Interval . recip . fromInterval

otone = simplify . Interval . (%1)
utone = simplify . Interval . (1%)

otones n = map otone . filter odd $ [1..n]
utones n = map utone . filter odd $ [1..n]

diamondList :: Int -> [Interval]
diamondList n = nub $ sort [x * y | x <- otones n, y <- utones n]

diamondArray :: Int -> Array (Int, Int) Interval
diamondArray n = listArray ((0,0), (n `div` 2, n `div` 2)) 
                 [otone x * utone y | x <- filter odd [1..n], y <- filter odd [1..n]]

showDiamond :: Array (Int, Int) Interval -> String
showDiamond d = concat . map showRow . showOffsets $ d
  where showRow r = rowPadding r ++ (concat $ map (showCol . (d!)) r) ++ "\n"
        maxCols = maximum . map length . showOffsets $ d
        rowPadding r = replicate ((maxCols - length r) * maxColSize `div` 2) ' '
        maxColSize = (maximum $ map (length . show) (elems d)) + 2
        showCol c = let colSize = length . show $ c
                        spacesLeft = (maxColSize - colSize) `div` 2
                        spacesRight = spacesLeft + (maxColSize - colSize) `mod` 2
                    in (replicate spacesLeft ' ') ++ show c ++ (replicate spacesRight ' ')
                       
showOffsets :: Array (Int, Int) Interval -> [[(Int, Int)]]
showOffsets d = map getRow [1..limit]
  where limit = (snd . snd . bounds $ d) * 2 + 1
        maxCols = limit `div` 2 + 1
        distFromCtr n = abs (maxCols - n)
        numCols n = maxCols - distFromCtr n
        swap (x,y) = (y,x)
        getRow n = let colFn = if n > maxCols
                               then colAt n
                               else swap . colAt n
                   in map colFn [0..numCols n - 1]
        colAt row col = (0 + col, distFromCtr row + col)
