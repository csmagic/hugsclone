-----------------------------------------------------------------------------
-- Number.hs:	Fixed width integers with overflow detection       (mpj 1996)
--
-- This library defines a numeric datatype of fixed width integers
-- (whatever Int supplies).  But, unlike Int, overflows are detected and
-- cause a run-time error.  Covers all classes upto and including Bounded
-- and Ix.  A fairly messy hack, but at least it works ...
--
-- Suitable for use with Hugs 1.3.
-----------------------------------------------------------------------------

module Number where

default (Number,Int,Float)

type Number = Int
  in numEq           :: Number -> Number -> Bool,
     numCmp          :: Number -> Number -> Ordering,
     numShowsPrec    :: Int -> Number -> ShowS,
     numEnumFrom     :: Number -> [Number],
     numEnumFromThen :: Number -> Number -> [Number],
     numAdd          :: Number -> Number -> Number,
     numSub          :: Number -> Number -> Number,
     numMul          :: Number -> Number -> Number,
     numNeg          :: Number -> Number,
     numFromInt      :: Int -> Number,
     numToInt        :: Number -> Int,
     numFromInteger  :: Integer -> Number,
     numToInteger    :: Number -> Integer,
     numMax          :: Number,
     numMin          :: Number,
     numToRat        :: Number -> Rational,
     numQrm          :: Number -> Number -> (Number, Number),
     numRange        :: (Number, Number) -> [Number],
     numIndex        :: (Number, Number) -> Number -> Int,
     numInRange      :: (Number, Number) -> Number -> Bool

numEq           = (==)
numCmp          = compare
numShowsPrec    = showsPrec
numEnumFrom     = enumFrom
numEnumFromThen = enumFromThen
numFromInt x    = x
numToInt x      = x
numFromInteger  = fromInteger
numToInteger    = toInteger
numMax          = maxBound
numMin          = minBound
numToRat        = toRational
numQrm          = quotRem
numRange        = range
numIndex        = index
numInRange      = inRange

numAdd x y = if xsgn/=ysgn || xsgn==rsgn then r else error "add overflow!"
             where xsgn = x>=0
                   ysgn = y>=0
                   rsgn = r>=0
                   r    = x + y

numSub x y = if xsgn==ysgn || ysgn/=rsgn then r else error "sub overflow!"
             where xsgn = x>=0
                   ysgn = y>=0
                   rsgn = r>=0
                   r    = x - y

numMul x y = if y==0 || (r `div` y == x) then r else error "mult overflow!"
             where r = x * y

numNeg x   = if x>=0 || r>=0 then r else error "negate overflow!"
             where r = negate x

instance Eq Number where
  (==)   = numEq

instance Ord Number where
  compare = numCmp

instance Show Number where
  showsPrec = numShowsPrec

instance Enum Number where
  toEnum       = numFromInt
  fromEnum     = numToInt
  enumFrom     = numEnumFrom
  enumFromThen = numEnumFromThen

instance Num Number where
  (+)         = numAdd
  (-)         = numSub
  (*)         = numMul
  negate      = numNeg
  fromInt     = numFromInt
  fromInteger = numFromInteger
  abs x       = if x<0 then negate x else x

instance Bounded Number where
  minBound    = numMin
  maxBound    = maxBound

instance Real Number where
  toRational  = numToRat

instance Ix Number where
  range   = numRange
  index   = numIndex
  inRange = numInRange

instance Integral Number where
  quotRem   = numQrm
  toInteger = numToInteger

-----------------------------------------------------------------------------
