{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Partch where
import Data.Array
import Data.List (intercalate, nub, sort)
import Data.Ratio

newtype Interval = Interval { fromInterval :: Ratio Int }
              deriving (Eq, Num, Ord)
                       
instance Show Interval where
  show (Interval r) = show (numerator r) ++ ":" ++ show (denominator r)

type Diamond = Array (Int, Int) Interval

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
isSimple i = i >= 1 && i < 2

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

scale :: Int -> [Interval]
scale n = nub $ sort [x * y | x <- otones n, y <- utones n]
            
diamond n = let boundary    = ((0,0), (n `div` 2, n `div` 2))
                otonalities = sort $ map otone $ filter odd [1..n]
            in  listArray boundary [x * y | x <- otonalities, y <- map inverse otonalities]
            
limit :: Diamond -> Int
limit d = (snd . snd . bounds $ d) * 2 + 1

cents :: Interval -> Double
cents (Interval r) = let numer = fromIntegral . numerator $ r
                         denom = fromIntegral . denominator $ r
                     in 1200 * logBase 2 (numer / denom)

showDiamond :: Diamond -> String
showDiamond d = concat . map showRow . showOffsets $ d
  where showRow r    = rowPadding r ++ (concat $ map (showCol . (d!)) r) ++ "\n"
        maxCols      = maximum . map length . showOffsets $ d
        rowPadding r = replicate ((maxCols - length r) * maxColSize `div` 2) ' '
        maxColSize   = (maximum $ map (length . show) (elems d)) * 2
        showCol c    = let colSize = length . show $ c
                           spacesRight = (maxColSize - colSize) `div` 2
                           spacesLeft = spacesRight + (maxColSize - colSize) `mod` 2
                       in (replicate spacesLeft ' ') ++ show c ++ (replicate spacesRight ' ')
                       
showOffsets :: Diamond -> [[(Int, Int)]]
showOffsets d = map getRow [1..limit d]
  where maxCols       = limit d `div` 2 + 1
        distFromCtr n = abs (maxCols - n)
        numCols n     = maxCols - distFromCtr n
        swap (x,y)    = (y,x)
        getRow n      = let colFn = if n > maxCols
                                    then colAt n
                                    else swap . colAt n
                        in map colFn [0..numCols n - 1]
        colAt row col = (0 + col, distFromCtr row + col)
