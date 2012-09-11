{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Partch where
import Data.Array
import Data.List (nub, sort)
import Data.Ratio

-- | Intervals represented as integer ratios
newtype Interval = Interval { fromInterval :: Ratio Int }
              deriving (Eq, Num, Ord)
                       
instance Show Interval where
  show (Interval r) = show (numerator r) ++ ":" ++ show (denominator r)

-- | Tonality diamonds represented as 2-dimensional arrays
type Diamond = Array (Int, Int) Interval

-- | Increases an @Interval@ by a factor of one octave
octaveUp :: Interval -> Interval
octaveUp (Interval r) = Interval $ (2 * numerator r) % denominator r

-- | Decreases an @Interval@ by a factor of one octave
octaveDown :: Interval -> Interval
octaveDown (Interval r) = Interval $ numerator r % (denominator r * 2)

-- | Tests whether an @Interval@ is simple (between unison and an octave)
isSimple :: Interval -> Bool
isSimple i = i >= 1 && i < 2

-- | Places an @Interval@ into the range between unison and one octave
simplify :: Interval -> Interval
simplify i = if isSimple i
             then i
             else simplify $ if i < 1 then octaveUp i else octaveDown i

-- | Returns and simplifies the intervallic inverse of an @Interval@
inverse :: Interval -> Interval
inverse = simplify . Interval . recip . fromInterval

-- | Returns the n-th otonality (e.g. otone 3 = 3:2)
otone :: Int -> Interval
otone = simplify . Interval . (%1)

-- | Returns the n-th utonality (e.g. utone 3 = 4:3)
utone :: Int -> Interval
utone = simplify . Interval . (1%)

-- | Returns the list of n-limit otonalities
otones :: Int -> [Interval]
otones n = map otone . filter odd $ [1..n]

-- | Returns the list of n-limit utonalities
utones :: Int -> [Interval]
utones n = map utone . filter odd $ [1..n]

-- | Returns a sorted list of unique n-limit @Interval@s
scale :: Int -> [Interval]
scale n = nub $ sort [simplify $ x * y | x <- otones n, y <- utones n]
            
-- | Returns the n-limit tonality @Diamond@
diamond :: Int -> Diamond
diamond n = let boundary    = ((0,0), (n `div` 2, n `div` 2))
                otonalities = sort $ map otone $ filter odd [1..n]
            in listArray boundary [simplify $ x * y | x <- otonalities,
                                                      y <- map inverse otonalities]
            
-- | Determines the n-limit of the @Diamond@ based on its array bounds
limit :: Diamond -> Int
limit d = (snd . snd . bounds $ d) * 2 + 1

-- | Converts an @Interval@ to cents
cents :: Interval -> Double
cents (Interval r) = let numer = fromIntegral . numerator $ r
                         denom = fromIntegral . denominator $ r
                     in 1200 * logBase 2 (numer / denom)

-- | Produces a text representation of a @Diamond@ suitable for displaying in a terminal
showDiamond :: Diamond -> String
showDiamond d = concat . map showRow . diamondOffsets $ d
  where showRow r    = rowPadding r ++ (concat $ map (showCol . (d!)) r) ++ "\n"
        maxCols      = maximum . map length . diamondOffsets $ d
        rowPadding r = replicate ((maxCols - length r) * maxColSize `div` 2) ' '
        maxColSize   = (maximum $ map (length . show) (elems d)) * 2
        showCol c    = let colSize     = length . show $ c
                           spacesRight = (maxColSize - colSize) `div` 2
                           spacesLeft  = spacesRight + (maxColSize - colSize) `mod` 2
                       in (replicate spacesLeft ' ') ++ show c ++ (replicate spacesRight ' ')
                       
-- | Produces a list of lists of @Diamond@ array indices, suitable for passing to 
-- visual rendering function
diamondOffsets :: Diamond -> [[(Int, Int)]]
diamondOffsets d = map getRow [1..limit d]
  where maxCols       = limit d `div` 2 + 1
        distFromCtr n = abs (maxCols - n)
        numCols n     = maxCols - distFromCtr n
        swap (x,y)    = (y,x)
        getRow n      = let colFn = if n > maxCols
                                    then colAt n
                                    else swap . colAt n
                        in map colFn [0..numCols n - 1]
        colAt row col = (0 + col, distFromCtr row + col)
