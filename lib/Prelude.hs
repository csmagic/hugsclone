{----------------------------------------------------------------------------
      ___    ___   ___    ___   __________   __________
     /  /   /  /  /  /   /  /  /  _______/  /  _______/    The Haskell User's
    /  /___/  /  /  /   /  /  /  / _____   /  /______         Gofer System
   /  ____   /  /  /   /  /  /  / /_   /  /______   /
  /  /   /  /  /  /___/  /  /  /___/  /  _______/  /           Version 1.3
 /__/   /__/  /_________/  /_________/  /_________/             July 1996

 Copyright (c) Mark P Jones, The University of Nottingham, 1994-1996.

 This is the Hugs Standard Prelude, based very closely on the Standard
 Prelude for Haskell 1.3.

 WARNING: This file is an integral part of the Hugs source code.  Changes to
 the definitions in this file without corresponding modifications in other
 parts of the program may cause the interpreter to fail unexpectedly.  Under
 normal circumstances, you should not attempt to modify this file in any way!

-----------------------------------------------------------------------------
     Hugs is subject to conditions of use and distribution; see the file
      "NOTICE" included with the main distribution for further details.
----------------------------------------------------------------------------}

-- Standard value bindings {Prelude} ----------------------------------------

infixr 9  .
infixl 9  !!
infixr 8  ^, ^^, **
infixl 7  *, /, `quot`, `rem`, `div`, `mod`, :%, %
infixl 6  +, -
infixr 5  :, ++
infix  4  ==, /=, <, <=, >=, >, `elem`, `notElem`
infixr 3  &&
infixr 2  ||
infixr 1  >>, >>=
infixr 0  $, `seq`

-- Equality and Ordered classes ---------------------------------------------

class Eq a where
    (==), (/=) :: a -> a -> Bool
    x /= y      = not (x==y)

class (Eq a) => Ord a where
    compare                :: a -> a -> Ordering
    (<), (<=), (>=), (>)   :: a -> a -> Bool
    max, min               :: a -> a -> a

    compare x y | x==y      = EQ
		| x<=y      = LT
		| otherwise = GT

    x <= y                  = compare x y /= GT
    x <  y                  = compare x y == LT
    x >= y                  = compare x y /= LT
    x >  y                  = compare x y == GT

    max x y   | x >= y      = x
	      | otherwise   = y
    min x y   | x <= y      = x
	      | otherwise   = y

class Ord a => Bounded a where
    minBound, maxBound :: a

-- Numeric classes ----------------------------------------------------------

class (Eq a, Show a, Eval a) => Num a where
    (+), (-), (*)  :: a -> a -> a
    negate         :: a -> a
    abs, signum    :: a -> a
    fromInteger    :: Integer -> a
    fromInt        :: Int -> a

    x - y           = x + negate y

class (Num a, Enum a) => Real a where
    toRational     :: a -> Rational

class (Real a, Ix a) => Integral a where
    quot, rem, div, mod :: a -> a -> a
    quotRem, divMod     :: a -> a -> (a,a)
    even, odd           :: a -> Bool
    toInteger           :: a -> Integer
    toInt               :: a -> Int

    n `quot` d           = q where (q,r) = quotRem n d
    n `rem` d            = r where (q,r) = quotRem n d
    n `div` d            = q where (q,r) = divMod n d
    n `mod` d            = r where (q,r) = divMod n d
    divMod n d           = if signum r == - signum d then (q-1, r+d) else qr
			   where qr@(q,r) = quotRem n d
    even n               = n `rem` 2 == 0
    odd                  = not . even

class (Num a) => Fractional a where
    (/)          :: a -> a -> a
    recip        :: a -> a
    fromRational :: Rational -> a
    fromDouble   :: Double -> a

    recip x       = 1 / x

class (Fractional a) => Floating a where
    pi                  :: a
    exp, log, sqrt      :: a -> a
    (**), logBase       :: a -> a -> a
    sin, cos, tan       :: a -> a
    asin, acos, atan    :: a -> a
    sinh, cosh, tanh    :: a -> a
    asinh, acosh, atanh :: a -> a

    x ** y               = exp (log x * y)
    logBase x y          = log y / log x
    sqrt x               = x ** 0.5
    tan x                = sin x / cos x
    sinh x               = (exp x - exp (-x)) / 2
    cosh x               = (exp x + exp (-x)) / 2
    tanh x               = sinh x / cosh x
    asinh x              = log (x + sqrt (x*x + 1))
    acosh x              = log (x + sqrt (x*x - 1))
    atanh x              = (log (1 + x) - log (1 - x)) / 2

class (Real a, Fractional a) => RealFrac a where
    properFraction   :: (Integral b) => a -> (b,a)
    truncate, round  :: (Integral b) => a -> b
    ceiling, floor   :: (Integral b) => a -> b

    truncate x        = m where (m,_) = properFraction x

    round x           = let (n,r) = properFraction x
			    m     = if r < 0 then n - 1 else n + 1
			in case signum (abs r - 0.5) of
			    -1 -> n
			    0  -> if even n then n else m
			    1  -> m

    ceiling x         = if r > 0 then n + 1 else n
			where (n,r) = properFraction x

    floor x           = if r < 0 then n - 1 else n
			where (n,r) = properFraction x

class (RealFrac a, Floating a) => RealFloat a where
    floatRadix       :: a -> Integer
    floatDigits      :: a -> Int
    floatRange       :: a -> (Int,Int)
    decodeFloat      :: a -> (Integer,Int)
    encodeFloat      :: Integer -> Int -> a
    exponent         :: a -> Int
    significand      :: a -> a
    scaleFloat       :: Int -> a -> a
    isNaN, isInfinite, isDenormalized, isNegativeZero, isIEEE
		     :: a -> Bool

    exponent x        = if m==0 then 0 else n + floatDigits x
			where (m,n) = decodeFloat x
    significand x     = encodeFloat m (- floatDigits x)
			where (m,_) = decodeFloat x
    scaleFloat k x    = encodeFloat m (n+k)
			where (m,n) = decodeFloat x

-- Numeric functions --------------------------------------------------------

subtract       :: Num a => a -> a -> a
subtract        = flip (-)

gcd            :: Integral a => a -> a -> a
gcd 0 0         = error "Prelude.gcd: gcd 0 0 is undefined"
gcd x y         = gcd' (abs x) (abs y)
		  where gcd' x 0 = x
			gcd' x y = gcd' y (x `rem` y)

lcm            :: (Integral a) => a -> a -> a
lcm _ 0         = 0
lcm 0 _         = 0
lcm x y         = abs ((x `quot` gcd x y) * y)

(^)            :: (Num a, Integral b) => a -> b -> a
x ^ 0           = 1
x ^ n  | n > 0  = f x (n-1) x
		  where f _ 0 y = y
			f x n y = g x n where
				  g x n | even n    = g (x*x) (n`quot`2)
					| otherwise = f x (n-1) (x*y)
_ ^ _           = error "Prelude.^: negative exponent"

(^^)           :: (Fractional a, Integral b) => a -> b -> a
x ^^ n          = if n >= 0 then x ^ n else recip (x^(-n))

fromIntegral   :: (Integral a, Num b) => a -> b
fromIntegral    = fromInteger . toInteger

fromRealFrac   :: (RealFrac a, Fractional b) => a -> b
fromRealFrac    = fromRational . toRational

atan2          :: (RealFloat a) => a -> a -> a
atan2 y x       = case (signum y, signum x) of
		    ( 0, 1) ->  0
		    ( 1, 0) ->  pi/2
		    ( 0,-1) ->  pi
		    (-1, 0) -> -pi/2
		    ( _, 1) -> atan (y/x)
		    ( _,-1) -> atan (y/x) + pi
		    ( 0, 0) -> error "Prelude.atan2: atan2 of origin"

-- Index and Enumeration classes --------------------------------------------

class (Ord a) => Ix a where
    range                :: (a,a) -> [a]
    index                :: (a,a) -> a -> Int
    inRange              :: (a,a) -> a -> Bool
    rangeSize            :: (a,a) -> Int

    rangeSize r@(l,u)     = index r u + 1


class (Ord a) => Enum a where
    toEnum               :: Int -> a
    fromEnum             :: a -> Int
    enumFrom             :: a -> [a]              -- [n..]
    enumFromThen         :: a -> a -> [a]         -- [n,m..]
    enumFromTo           :: a -> a -> [a]         -- [n..m]
    enumFromThenTo       :: a -> a -> a -> [a]    -- [n,n'..m]

    enumFromTo n m        = takeWhile (m>=) (enumFrom n)
    enumFromThenTo n n' m = takeWhile ((if n'>=n then (>=) else (<=)) m)
				      (enumFromThen n n')

succ, pred :: Enum a => a -> a
succ        = toEnum . (1+)       . fromEnum
pred        = toEnum . subtract 1 . fromEnum

-- Read and Show classes ------------------------------------------------------

type ReadS a = String -> [(a,String)]
type ShowS   = String -> String

class Read a where
    readsPrec :: Int -> ReadS a
    readList  :: ReadS [a]
    readList   = readParen False (\r -> [pr | ("[",s) <- lex r,
					      pr      <- readl s ])
		 where readl  s = [([],t)   | ("]",t) <- lex s] ++
				  [(x:xs,u) | (x,t)   <- reads s,
					      (xs,u)  <- readl' t]
		       readl' s = [([],t)   | ("]",t) <- lex s] ++
				  [(x:xs,v) | (",",t) <- lex s,
					      (x,u)   <- reads t,
					      (xs,v)  <- readl' u]

class Show a where
    showsPrec :: Int -> a -> ShowS
    showList  :: [a] -> ShowS

    showList []     = showString "[]"
    showList (x:xs) = showChar '[' . shows x . showl xs
		      where showl []     = showChar ']'
			    showl (x:xs) = showString ", " . shows x . showl xs

-- Monad classes ------------------------------------------------------------

class Functor f where
    map :: (a -> b) -> (f a -> f b)

class Monad m where
    (>>=)  :: m a -> (a -> m b) -> m b
    return :: a -> m a

    (>>)   :: m a -> m b -> m b
    p >> q  = do {p;q}

class Monad m => MonadZero m where
    zero   :: m a

class MonadZero m => MonadPlus m where
    (++)   :: m a -> m a -> m a

accumulate       :: Monad m => [m a] -> m [a]
accumulate []     = return []
accumulate (c:cs) = do x  <- c
		       xs <- accumulate cs
		       return (x:xs)

sequence         :: Monad m => [m a] -> m ()
sequence          = foldr (>>) done

mapM             :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f            = accumulate . map f

mapM_            :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ f           = sequence . map f

guard            :: MonadZero m => Bool -> m ()
guard p           = if p then done else zero

done             :: Monad m => m ()
done              = return ()

filter           :: MonadZero m => (a -> Bool) -> m a -> m a
filter p          = applyM (\x -> if p x then return x else zero)

concat           :: MonadPlus m => [m a] -> m a
concat            = foldr (++) zero

applyM           :: Monad m => (a -> m b) -> m a -> m b
applyM f x        = x >>= f

-- Evaluation and strictness ------------------------------------------------

class Eval a where
    strict :: (a -> b) -> a -> b
    seq    :: a -> b -> b

-- Void type ----------------------------------------------------------------

data Void      -- Void type has only one element, namely bottom.

-- Trivial type -------------------------------------------------------------

-- data () = () deriving (Eq, Ord, Ix, Enum, Read, Show, Bounded)

instance Eq () where
    () == ()  =  True

instance Ord () where
    compare () () = EQ

instance Ix () where
    range ((),())      = [()]
    index ((),()) ()   = 0
    inRange ((),()) () = True

instance Enum () where
    toEnum 0           = ()
    fromEnum ()        = 0
    enumFrom ()        = [()]
    enumFromThen () () = [()]

instance Read () where
    readsPrec p = readParen False (\r -> [((),t) | ("(",s) <- lex r,
						   (")",t) <- lex s ])

instance Show () where
    showsPrec p () = showString "()"

instance Bounded () where
    minBound = ()
    maxBound = ()

-- Boolean type -------------------------------------------------------------

data Bool    = False | True
	       deriving (Eq, Ord, Ix, Enum, Read, Show, Bounded)

(&&), (||)  :: Bool -> Bool -> Bool
False && x   = False
True  && x   = x
False || x   = x
True  || x   = True

not         :: Bool -> Bool
not True     = False
not False    = True

otherwise   :: Bool
otherwise    = True

-- Character type -----------------------------------------------------------

data Char               -- builtin datatype of ISO Latin characters
type String = [Char]    -- strings are lists of characters

primitive primEqChar    :: Char -> Char -> Bool
primitive primCmpChar   :: Char -> Char -> Ordering

instance Eq Char  where (==)    = primEqChar
instance Ord Char where compare = primCmpChar

primitive ord "primCharToInt" :: Char -> Int
primitive chr "primIntToChar" :: Int -> Char

instance Enum Char where
    toEnum           = chr
    fromEnum         = ord
    enumFrom c       = map chr [ord c .. ord maxBound]
    enumFromThen c d = map chr [ord c, ord d .. ord lastChar]
		       where lastChar = if d < c then minBound else maxBound

instance Ix Char where
    range (c,c')      = [c..c']
    index b@(c,c') ci
       | inRange b ci = ord ci - ord c
       | otherwise    = error "index: Index out of range"
    inRange (c,c') ci = ord c <= i && i <= ord c'
			where i = ord ci

instance Read Char where
    readsPrec p      = readParen False
			    (\r -> [(c,t) | ('\'':s,t) <- lex r,
					    (c,_)      <- readLitChar s])
    readList = readParen False (\r -> [(l,t) | ('"':s, t) <- lex r,
					       (l,_)      <- readl s ])
	       where readl ('"':s)      = [("",s)]
		     readl ('\\':'&':s) = readl s
		     readl s            = [(c:cs,u) | (c ,t) <- readLitChar s,
						      (cs,u) <- readl t ]
instance Show Char where
    showsPrec p '\'' = showString "'\\''"
    showsPrec p c    = showChar '\'' . showLitChar c . showChar '\''

    showList cs   = showChar '"' . showl cs
		    where showl ""       = showChar '"'
			  showl ('"':cs) = showString "\\\"" . showl cs
			  showl (c:cs)   = showLitChar c . showl cs

instance Bounded Char where
    minBound = '\0'
    maxBound = '\255'

isAscii, isControl, isPrint, isSpace            :: Char -> Bool
isUpper, isLower, isAlpha, isDigit, isAlphanum  :: Char -> Bool

isAscii c              =  ord c < 128
isControl c            =  c < ' ' ||  c == '\DEL'
isPrint c              =  c >= ' ' &&  c <= '~'
isSpace c              =  c == ' ' || c == '\t' || c == '\n' ||
			  c == '\r' || c == '\f' || c == '\v'
isUpper c              =  c >= 'A'   &&  c <= 'Z'
isLower c              =  c >= 'a'   &&  c <= 'z'
isAlpha c              =  isUpper c  ||  isLower c
isDigit c              =  c >= '0'   &&  c <= '9'
isAlphanum c           =  isAlpha c  ||  isDigit c

toUpper, toLower      :: Char -> Char
toUpper c | isLower c  = chr (ord c - ord 'a' + ord 'A')
	  | otherwise  = c

toLower c | isUpper c  = chr (ord c - ord 'A' + ord 'a')
	  | otherwise  = c

-- Maybe type ---------------------------------------------------------------

data Maybe a = Just a | Nothing
	       deriving (Eq, Ord, Read, Show)

maybe             :: b -> (a -> b) -> Maybe a -> b
maybe n f Nothing  = n
maybe n f (Just x) = f x

instance Functor Maybe where
    map f Nothing  = Nothing
    map f (Just x) = Just (f x)

instance Monad Maybe where
    Just x  >>= k  =  k x
    Nothing >>= k  =  Nothing
    return         =  Just

instance MonadZero Maybe where
    zero           = Nothing

instance MonadPlus Maybe where
    Nothing ++ ys  = ys
    xs      ++ ys  = xs

-- Either type --------------------------------------------------------------

data Either a b = Left a | Right b
		  deriving (Eq, Ord, Read, Show)

either              :: (a -> c) -> (b -> c) -> Either a b -> c
either l r (Left x)  = l x
either l r (Right y) = r y

-- Ordering type ------------------------------------------------------------

data Ordering = LT | EQ | GT
		deriving (Eq, Ord, Ix, Enum, Read, Show, Bounded)

-- Lists --------------------------------------------------------------------

-- data [a] = [] | a : [a] deriving (Eq, Ord)

instance Eq a => Eq [a] where
    []     == []     =  True
    (x:xs) == (y:ys) =  x==y && xs==ys
    _      == _      =  False

instance Ord a => Ord [a] where
    compare []     (_:_)  = LT
    compare []     []     = EQ
    compare (_:_)  []     = GT
    compare (x:xs) (y:ys) = primCompAux x y (compare xs ys)

instance Functor [] where
    map f []     = []
    map f (x:xs) = f x : map f xs

instance Monad [ ] where
    (x:xs) >>= f  =  f x ++ (xs >>= f)
    []     >>= f  =  []
    return x      =  [x]

instance MonadZero [ ] where
    zero = []

instance MonadPlus [ ] where
    []     ++ ys = ys
    (x:xs) ++ ys = x : (xs ++ ys)

instance Read a => Read [a]  where
    readsPrec p = readList

instance Show a => Show [a]  where
    showsPrec p = showList

-- Tuples -------------------------------------------------------------------

-- data (a,b) = (a,b) deriving (Eq, Ord, Ix, Read, Show)
-- etc..

-- Functions ----------------------------------------------------------------

instance Show (a -> b) where
    showsPrec p f = showString "<<function>>"

instance Functor ((->) a) where
    map f g x = f (g x)

-- Standard Integral types --------------------------------------------------

data Int      -- builtin datatype of fixed size integers
data Integer  -- builtin datatype of arbitrary size integers

primitive primEqInt      :: Int -> Int -> Bool
primitive primCmpInt     :: Int -> Int -> Ordering
primitive primEqInteger  :: Integer -> Integer -> Bool
primitive primCmpInteger :: Integer -> Integer -> Ordering

instance Eq  Int     where (==)    = primEqInt
instance Eq  Integer where (==)    = primEqInteger
instance Ord Int     where compare = primCmpInt
instance Ord Integer where compare = primCmpInteger

primitive primPlusInt,
	  primMinusInt,
	  primMulInt	   :: Int -> Int -> Int
primitive primNegInt	   :: Int -> Int
primitive primIntegerToInt :: Integer -> Int

instance Num Int where
    (+)           = primPlusInt
    (-)           = primMinusInt
    negate        = primNegInt
    (*)           = primMulInt
    abs           = absReal
    signum        = signumReal
    fromInteger   = primIntegerToInt
    fromInt x     = x

primitive primMinInt, primMaxInt :: Int

instance Bounded Int where
    minBound = primMinInt
    maxBound = primMaxInt

primitive primPlusInteger,
	  primMinusInteger,
	  primMulInteger   :: Integer -> Integer -> Integer
primitive primNegInteger   :: Integer -> Integer
primitive primIntToInteger :: Int -> Integer

instance Num Integer where
    (+)           = primPlusInteger
    (-)           = primMinusInteger
    negate        = primNegInteger
    (*)           = primMulInteger
    abs           = absReal
    signum        = signumReal
    fromInteger x = x
    fromInt       = primIntToInteger

absReal x    | x >= 0    = x
	     | otherwise = -x

signumReal x | x == 0    =  0
	     | x > 0     =  1
	     | otherwise = -1

instance Real Int where
    toRational x = toInteger x % 1

instance Real Integer where
    toRational x = x % 1

primitive primDivInt,
	  primQuotInt,
	  primRemInt,
	  primModInt  :: Int -> Int -> Int
primitive primQrmInt  :: Int -> Int -> (Int,Int)
primitive primEvenInt :: Int -> Bool

instance Integral Int where
    div       = primDivInt
    quot      = primQuotInt
    rem       = primRemInt
    mod       = primModInt
    quotRem   = primQrmInt
    even      = primEvenInt
    toInteger = primIntToInteger
    toInt x   = x

primitive primQrmInteger  :: Integer -> Integer -> (Integer,Integer)
primitive primEvenInteger :: Integer -> Bool

instance Integral Integer where
    quotRem     = primQrmInteger
    even        = primEvenInteger
    toInteger x = x
    toInt       = primIntegerToInt

instance Ix Int where
    range (m,n)          = [m..n]
    index b@(m,n) i
	   | inRange b i = i - m
	   | otherwise   = error "index: Index out of range"
    inRange (m,n) i      = m <= i && i <= n

instance Ix Integer where
    range (m,n)          = [m..n]
    index b@(m,n) i
	   | inRange b i = fromInteger (i - m)
	   | otherwise   = error "index: Index out of range"
    inRange (m,n) i      = m <= i && i <= n

instance Enum Int where
    toEnum       = id
    fromEnum     = id
    enumFrom n   = n : strict enumFrom (n+1) -- not numericEnumFrom
    enumFromThen = numericEnumFromThen

instance Enum Integer where
    toEnum       = primIntToInteger
    fromEnum     = primIntegerToInt
    enumFrom     = numericEnumFrom
    enumFromThen = numericEnumFromThen

numericEnumFrom        :: Real a => a -> [a]
numericEnumFromThen    :: Real a => a -> a -> [a]
numericEnumFrom         = iterate (1+)
numericEnumFromThen n m = iterate ((m-n)+) n

primitive primShowsInt :: Int -> Int -> ShowS

instance Read Int where
    readsPrec p = readSigned readDec

instance Show Int where
    showsPrec   = primShowsInt

primitive primShowsInteger :: Int -> Integer -> ShowS

instance Read Integer where
    readsPrec p = readSigned readDec

instance Show Integer where
    showsPrec   = primShowsInteger

-- Standard Floating types --------------------------------------------------

data Float     -- builtin datatype of single precision floating point numbers
data Double    -- builtin datatype of double precision floating point numbers

primitive primEqFloat   :: Float -> Float -> Bool
primitive primCmpFloat  :: Float -> Float -> Ordering
primitive primEqDouble  :: Double -> Double -> Bool
primitive primCmpDouble :: Double -> Double -> Ordering

instance Eq  Float  where (==) = primEqFloat
instance Eq  Double where (==) = primEqDouble

instance Ord Float  where compare = primCmpFloat
instance Ord Double where compare = primCmpDouble

primitive primPlusFloat,
	  primMinusFloat,
	  primMulFloat       :: Float -> Float -> Float
primitive primNegFloat       :: Float -> Float
primitive primIntToFloat     :: Int -> Float
primitive primIntegerToFloat :: Integer -> Float

instance Num Float where
    (+)           = primPlusFloat
    (-)           = primMinusFloat
    negate        = primNegFloat
    (*)           = primMulFloat
    abs           = absReal
    signum        = signumReal
    fromInteger   = primIntegerToFloat
    fromInt       = primIntToFloat

primitive primPlusDouble,
	  primMinusDouble,
	  primMulDouble       :: Double -> Double -> Double
primitive primNegDouble       :: Double -> Double
primitive primIntToDouble     :: Int -> Double
primitive primIntegerToDouble :: Integer -> Double

instance Num Double where
    (+)         = primPlusDouble
    (-)         = primMinusDouble
    negate      = primNegDouble
    (*)         = primMulDouble
    abs         = absReal
    signum      = signumReal
    fromInteger = primIntegerToDouble
    fromInt     = primIntToDouble

instance Real Float where
    toRational = realFloatToRational

instance Real Double where
    toRational = realFloatToRational

realFloatToRational x = (m%1)*(b%1)^^n
			where (m,n) = decodeFloat x
			      b     = floatRadix x

primitive primDivFloat      :: Float -> Float -> Float
primitive primDoubleToFloat :: Double -> Float

instance Fractional Float where
    (/)          = primDivFloat
    fromRational = rationalToRealFloat
    fromDouble   = primDoubleToFloat

primitive primDivDouble :: Double -> Double -> Double

instance Fractional Double where
    (/)          = primDivDouble
    fromRational = rationalToRealFloat
    fromDouble x = x

rationalToRealFloat x = x'
 where x'    = f e
       f e   = if e' == e then y else f e'
	       where y      = encodeFloat (round (x * (1%b)^^e)) e
		     (_,e') = decodeFloat y
       (_,e) = decodeFloat (fromInteger (numerator x) `asTypeOf` x'
			     / fromInteger (denominator x))
       b     = floatRadix x'

primitive primPiFloat :: Float
primitive primSinFloat,  primAsinFloat, primCosFloat,
	  primAcosFloat, primTanFloat,  primAtanFloat,
	  primLogFloat,  primExpFloat,  primSqrtFloat :: Float -> Float

instance Floating Float where
    pi    = primPiFloat
    exp   = primExpFloat
    log   = primLogFloat
    sqrt  = primSqrtFloat
    sin   = primSinFloat
    cos   = primCosFloat
    tan   = primTanFloat
    asin  = primAsinFloat
    acos  = primAcosFloat
    atan  = primAtanFloat

primitive primPiDouble :: Double
primitive primSinDouble,  primAsinDouble, primCosDouble,
	  primAcosDouble, primTanDouble,  primAtanDouble,
	  primLogDouble,  primExpDouble,  primSqrtDouble :: Double -> Double

instance Floating Double where
    pi    = primPiDouble
    exp   = primExpDouble
    log   = primLogDouble
    sqrt  = primSqrtDouble
    sin   = primSinDouble
    cos   = primCosDouble
    tan   = primTanDouble
    asin  = primAsinDouble
    acos  = primAcosDouble
    atan  = primAtanDouble

instance RealFrac Float where
    properFraction = floatProperFraction

instance RealFrac Double where
    properFraction = floatProperFraction

floatProperFraction x
   | n >= 0      = (fromInteger m * fromInteger b ^ n, 0)
   | otherwise   = (fromInteger w, encodeFloat r n)
		   where (m,n) = decodeFloat x
			 b     = floatRadix x
			 (w,r) = quotRem m (b^(-n))

primitive primFloatRadix  :: Float -> Integer
primitive primFloatDigits :: Float -> Int
primitive primFloatRange  :: Float -> (Int,Int)
primitive primFloatEncode :: Integer -> Int -> Float
primitive primFloatDecode :: Float -> (Integer, Int)

instance RealFloat Float where
    floatRadix  = primFloatRadix
    floatDigits = primFloatDigits
    floatRange  = primFloatRange
    encodeFloat = primFloatEncode
    decodeFloat = primFloatDecode

primitive primDoubleRadix  :: Double -> Integer
primitive primDoubleDigits :: Double -> Int
primitive primDoubleRange  :: Double -> (Int,Int)
primitive primDoubleEncode :: Integer -> Int -> Double
primitive primDoubleDecode :: Double -> (Integer, Int)

instance RealFloat Double where
    floatRadix  = primDoubleRadix
    floatDigits = primDoubleDigits
    floatRange  = primDoubleRange
    encodeFloat = primDoubleEncode
    decodeFloat = primDoubleDecode

instance Enum Float where
    toEnum       = primIntToFloat
    fromEnum     = truncate
    enumFrom     = numericEnumFrom
    enumFromThen = numericEnumFromThen

instance Enum Double where
    toEnum       = primIntToDouble
    fromEnum     = truncate
    enumFrom     = numericEnumFrom
    enumFromThen = numericEnumFromThen

primitive primShowsFloat :: Int -> Float -> ShowS

instance Read Float where
    readsPrec p = readSigned readFloat

instance Show Float where
    showsPrec   = primShowsFloat

primitive primShowsDouble :: Int -> Double -> ShowS

instance Read Double where
    readsPrec p = readSigned readFloat

instance Show Double where
    showsPrec   = primShowsDouble

-- Some standard functions --------------------------------------------------

fst            :: (a,b) -> a
fst (x,_)       = x

snd            :: (a,b) -> b
snd (_,y)       = y

curry          :: ((a,b) -> c) -> (a -> b -> c)
curry f x y     = f (x,y)

uncurry        :: (a -> b -> c) -> ((a,b) -> c)
uncurry f p     = f (fst p) (snd p)

id             :: a -> a
id    x         = x

const          :: a -> b -> a
const k _       = k

(.)            :: (b -> c) -> (a -> b) -> (a -> c)
(f . g) x       = f (g x)

--(.)          :: Functor f => (a -> b) -> (f a -> f b)
--(.)           = map

flip           :: (a -> b -> c) -> b -> a -> c
flip f x y      = f y x

($)            :: (a -> b) -> a -> b
f $ x           = f x

until          :: (a -> Bool) -> (a -> a) -> a -> a
until p f x     = if p x then x else until p f (f x)

asTypeOf       :: a -> a -> a
asTypeOf        = const

primitive error  :: String -> a

undefined        :: a
undefined | False = undefined

-- Standard functions on rational numbers {PreludeRatio} --------------------

data Integral a => Ratio a = a :% a deriving (Eq)
type Rational              = Ratio Integer

(%)                       :: Integral a => a -> a -> Ratio a
x % y                      = reduce (x * signum y) (abs y)

reduce                    :: Integral a => a -> a -> Ratio a
reduce x y | y == 0        = error "PreludeRatio.%: zero denominator"
	   | otherwise     = (x `quot` d) :% (y `quot` d)
			     where d = gcd x y

numerator, denominator    :: Integral a => Ratio a -> a
numerator (x :% y)         = x
denominator (x :% y)       = y

instance Integral a => Ord (Ratio a) where
    compare (x:%y) (x':%y') = compare (x*y') (x'*y)

instance Integral a => Num (Ratio a) where
    (x:%y) + (x':%y') = reduce (x*y' + x'*y) (y*y')
    (x:%y) * (x':%y') = reduce (x*x') (y*y')
    negate (x :% y)   = negate x :% y
    abs (x :% y)      = abs x :% y
    signum (x :% y)   = signum x :% 1
    fromInteger x     = fromInteger x :% 1
    fromInt x         = fromInt x :% 1

instance Integral a => Real (Ratio a) where
    toRational (x:%y) = toInteger x :% toInteger y

instance Integral a => Fractional (Ratio a) where
    (x:%y) / (x':%y')   = (x*y') % (y*x')
    recip (x:%y)        = if x < 0 then (-y) :% (-x) else y :% x
    fromRational (x:%y) = fromInteger x :% fromInteger y
    fromDouble x
	    | n>=0      = (fromInteger m * fromInteger b ^ n) % 1
	    | otherwise = fromInteger m % (fromInteger b ^ (-n))
			  where (m,n) = decodeFloat x
				b     = floatRadix x

instance Integral a => RealFrac (Ratio a) where
    properFraction (x:%y) = (fromIntegral q, r:%y)
			    where (q,r) = quotRem x y

instance Integral a => Enum (Ratio a) where
    toEnum       = fromInt
    fromEnum     = truncate
    enumFrom     = numericEnumFrom
    enumFromThen = numericEnumFromThen

instance (Read a, Integral a) => Read (Ratio a) where
    readsPrec p = readParen (p > 7)
			    (\r -> [(x%y,u) | (x,s)   <- reads r,
					      ("%",t) <- lex s,
					      (y,u)   <- reads t ])

instance Integral a => Show (Ratio a) where
    showsPrec p (x:%y) = showParen (p > 7)
			     (shows x . showString " % " . shows y)

approxRational      :: RealFrac a => a -> a -> Rational
approxRational x eps = simplest (x-eps) (x+eps)
 where simplest x y | y < x     = simplest y x
		    | x == y    = xr
		    | x > 0     = simplest' n d n' d'
		    | y < 0     = - simplest' (-n') d' (-n) d
		    | otherwise = 0 :% 1
				  where xr@(n:%d) = toRational x
					(n':%d')  = toRational y
       simplest' n d n' d'        -- assumes 0 < n%d < n'%d'
		    | r == 0    = q :% 1
		    | q /= q'   = (q+1) :% 1
		    | otherwise = (q*n''+d'') :% n''
				  where (q,r)      = quotRem n d
					(q',r')    = quotRem n' d'
					(n'':%d'') = simplest' d' r' d r

-- Standard list functions {PreludeList} ------------------------------------

head             :: [a] -> a
head (x:_)        = x

last             :: [a] -> a
last [x]          = x
last (_:xs)       = last xs

tail             :: [a] -> [a]
tail (_:xs)       = xs

init             :: [a] -> [a]
init [x]          = []
init (x:xs)       = x : init xs

null             :: [a] -> Bool
null []           = True
null (_:_)        = False

length           :: [a] -> Int
length            = foldl' (\n _ -> n + 1) 0

(!!)             :: (Integral a) => [b] -> a -> b
(x:_)  !! 0       = x
(_:xs) !! n | n>0 = xs !! (n-1)
(_:_)  !! _       = error "PreludeList.!!: negative index"
[]     !! _       = error "PreludeList.!!: index too large"

foldl            :: (a -> b -> a) -> a -> [b] -> a
foldl f z []      = z
foldl f z (x:xs)  = foldl f (f z x) xs

foldl'           :: Eval a => (a -> b -> a) -> a -> [b] -> a
foldl' f a []     = a
foldl' f a (x:xs) = strict (foldl' f) (f a x) xs

foldl1           :: (a -> a -> a) -> [a] -> a
foldl1 f (x:xs)   = foldl f x xs

scanl            :: (a -> b -> a) -> a -> [b] -> [a]
scanl f q xs      = q : (case xs of
			 []   -> []
			 x:xs -> scanl f (f q x) xs)

scanl1           :: (a -> a -> a) -> [a] -> [a]
scanl1 f (x:xs)   = scanl f x xs

foldr            :: (a -> b -> b) -> b -> [a] -> b
foldr f z []      = z
foldr f z (x:xs)  = f x (foldr f z xs)

foldr1           :: (a -> a -> a) -> [a] -> a
foldr1 f [x]      = x
foldr1 f (x:xs)   = f x (foldr1 f xs)

scanr            :: (a -> b -> b) -> b -> [a] -> [b]
scanr f q0 []     = [q0]
scanr f q0 (x:xs) = f x q : qs
		    where qs@(q:_) = scanr f q0 xs

scanr1           :: (a -> a -> a) -> [a] -> [a]
scanr1 f [x]      = [x]
scanr1 f (x:xs)   = f x q : qs
		    where qs@(q:_) = scanr1 f xs

iterate          :: (a -> a) -> a -> [a]
iterate f x       = x : iterate f (f x)

repeat           :: a -> [a]
repeat x          = xs where xs = x:xs

replicate        :: Int -> a -> [a]
replicate n x     = take n (repeat x)

cycle            :: [a] -> [a]
cycle xs          = xs' where xs'=xs++xs'

take                :: Int -> [a] -> [a]
take 0 _             = []
take _ []            = []
take n (x:xs) | n>0  = x : take (n-1) xs
take _ _             = error "PreludeList.take: negative argument"

drop                :: Int -> [a] -> [a]
drop 0 xs            = xs
drop _ []            = []
drop n (_:xs) | n>0  = drop (n-1) xs
drop _ _             = error "PreludeList.drop: negative argument"

splitAt               :: Int -> [a] -> ([a], [a])
splitAt 0 xs           = ([],xs)
splitAt _ []           = ([],[])
splitAt n (x:xs) | n>0 = (x:xs',xs'') where (xs',xs'') = splitAt (n-1) xs
splitAt _ _            = error "PreludeList.splitAt: negative argument"

takeWhile           :: (a -> Bool) -> [a] -> [a]
takeWhile p []       = []
takeWhile p (x:xs)
	 | p x       = x : takeWhile p xs
	 | otherwise = []

dropWhile           :: (a -> Bool) -> [a] -> [a]
dropWhile p []       = []
dropWhile p xs@(x:xs')
	 | p x       = dropWhile p xs'
	 | otherwise = xs

span, break         :: (a -> Bool) -> [a] -> ([a],[a])
span p []            = ([],[])
span p xs@(x:xs')
	 | p x       = let (ys,zs) = span p xs' in (x:ys,zs)
	 | otherwise = ([],xs)
break p              = span (not . p)

lines     :: String -> [String]
lines ""   = []
lines s    = l : (if null s' then [] else lines (tail s'))
	     where (l, s') = break ('\n'==) s

words     :: String -> [String]
words s    = case dropWhile isSpace s of
		  "" -> []
		  s' -> w : words s''
			where (w,s'') = break isSpace s'

unlines   :: [String] -> String
unlines    = concat . map (\l -> l ++ "\n")

unwords   :: [String] -> String
unwords [] = []
unwords ws = foldr1 (\w s -> w ++ ' ':s) ws

reverse          :: [a] -> [a]
reverse           = foldl (flip (:)) []

and, or        :: [Bool] -> Bool
and             = foldr (&&) True
or              = foldr (||) False

any, all       :: (a -> Bool) -> [a] -> Bool
any p           = or  . map p
all p           = and . map p

elem, notElem    :: Eq a => a -> [a] -> Bool
elem              = any . (==)
notElem           = all . (/=)

lookup           :: Eq a => a -> [(a,b)] -> Maybe b
lookup k []       = Nothing
lookup k ((x,y):xys)
      | k==x      = Just y
      | otherwise = lookup k xys

sum, product     :: Num a => [a] -> a
sum               = foldl' (+) 0
product           = foldl' (*) 1

maximum, minimum :: Ord a => [a] -> a
maximum           = foldl1 max
minimum           = foldl1 min

concatMap        :: (a -> [b]) -> [a] -> [b]
concatMap f       = concat . map f

zip  :: [a] -> [b] -> [(a,b)]
zip   = zipWith  (\a b -> (a,b))

zip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3  = zipWith3 (\a b c -> (a,b,c))

zipWith                  :: (a->b->c) -> [a]->[b]->[c]
zipWith z (a:as) (b:bs)   = z a b : zipWith z as bs
zipWith _ _      _        = []

zipWith3                 :: (a->b->c->d) -> [a]->[b]->[c]->[d]
zipWith3 z (a:as) (b:bs) (c:cs)
			  = z a b c : zipWith3 z as bs cs
zipWith3 _ _ _ _          = []

unzip                    :: [(a,b)] -> ([a],[b])
unzip                     = foldr (\(a,b) ~(as,bs) -> (a:as, b:bs)) ([], [])

unzip3                   :: [(a,b,c)] -> ([a],[b],[c])
unzip3                    = foldr (\(a,b,c) ~(as,bs,cs) -> (a:as,b:bs,c:cs))
				  ([],[],[])

-- PreludeText ----------------------------------------------------------------

reads        :: Read a => ReadS a
reads         = readsPrec 0

shows        :: Show a => a -> ShowS
shows         = showsPrec 0

read         :: Read a => String -> a
read s        =  case [x | (x,t) <- reads s, ("","") <- lex t] of
		      [x] -> x
		      []  -> error "PreludeText.read: no parse"
		      _   -> error "PreludeText.read: ambiguous parse"

show         :: Show a => a -> String
show x        = shows x ""

showChar     :: Char -> ShowS
showChar      = (:)

showString   :: String -> ShowS
showString    = (++)

showParen    :: Bool -> ShowS -> ShowS
showParen b p = if b then showChar '(' . p . showChar ')' else p

showField    :: Show a => String -> a -> ShowS
showField m v = showString m . showChar '=' . shows v

readParen    :: Bool -> ReadS a -> ReadS a
readParen b g = if b then mandatory else optional
		where optional r  = g r ++ mandatory r
		      mandatory r = [(x,u) | ("(",s) <- lex r,
					     (x,t)   <- optional s,
					     (")",u) <- lex t    ]

lex                     :: ReadS String
lex ""                  = [("","")]
lex (c:s) | isSpace c   = lex (dropWhile isSpace s)
lex ('-':'-':s)         = case dropWhile (/= '\n') s of
				 '\n':t -> lex t
				 _      -> [] -- unterminated end-of-line
					      -- comment

lex ('{':'-':s)         = lexNest lex s
			  where
			  lexNest f ('-':'}':s) = f s
			  lexNest f ('{':'-':s) = lexNest (lexNest f) s
			  lexNest f (c:s)             = lexNest f s
			  lexNest _ ""          = [] -- unterminated
						     -- nested comment

lex ('<':'-':s)         = [("<-",s)]
lex ('\'':s)            = [('\'':ch++"'", t) | (ch,'\'':t)  <- lexLitChar s,
					       ch /= "'"                ]
lex ('"':s)             = [('"':str, t)      | (str,t) <- lexString s]
			  where
			  lexString ('"':s) = [("\"",s)]
			  lexString s = [(ch++str, u)
						| (ch,t)  <- lexStrItem s,
						  (str,u) <- lexString t  ]

			  lexStrItem ('\\':'&':s) = [("\\&",s)]
			  lexStrItem ('\\':c:s) | isSpace c
			      = [("\\&",t) | '\\':t <- [dropWhile isSpace s]]
			  lexStrItem s            = lexLitChar s

lex (c:s) | isSingle c  = [([c],s)]
	  | isSym1 c    = [(c:sym,t)         | (sym,t) <- [span isSym s]]
	  | isAlpha c   = [(c:nam,t)         | (nam,t) <- [span isIdChar s]]
	  | isDigit c   = [(c:ds++fe,t)      | (ds,s)  <- [span isDigit s],
					       (fe,t)  <- lexFracExp s     ]
	  | otherwise   = []    -- bad character
		where
		isSingle c  =  c `elem` ",;()[]{}_"
		isSym1 c    =  c `elem` "-~" || isSym c
		isSym c     =  c `elem` "!@#$%&*+./<=>?\\^|:"
		isIdChar c  =  isAlphanum c || c `elem` "_'"

		lexFracExp ('.':s) = [('.':ds++e,u) | (ds,t) <- lexDigits s,
						      (e,u)  <- lexExp t    ]
		lexFracExp s       = [("",s)]

		lexExp (e:s) | e `elem` "eE"
			 = [(e:c:ds,u) | (c:t)  <- [s], c `elem` "+-",
						   (ds,u) <- lexDigits t] ++
			   [(e:ds,t)   | (ds,t) <- lexDigits s]
		lexExp s = [("",s)]

lexDigits               :: ReadS String
lexDigits               =  nonnull isDigit

nonnull                 :: (Char -> Bool) -> ReadS String
nonnull p s             =  [(cs,t) | (cs@(_:_),t) <- [span p s]]

lexLitChar              :: ReadS String
lexLitChar ('\\':s)     =  [('\\':esc, t) | (esc,t) <- lexEsc s]
	where
	lexEsc (c:s)     | c `elem` "abfnrtv\\\"'" = [([c],s)]
	lexEsc ('^':c:s) | c >= '@' && c <= '_'  = [(['^',c],s)]
	lexEsc s@(d:_)   | isDigit d             = lexDigits s
	lexEsc ('o':s)  =  [('o':os, t) | (os,t) <- nonnull isOctDigit s]
	lexEsc ('x':s)  =  [('x':xs, t) | (xs,t) <- nonnull isHexDigit s]
	lexEsc s@(c:_)   | isUpper c
			=  case [(mne,s') | mne <- "DEL" : map snd asciiTab,
					    ([],s') <- [lexmatch mne s]      ]
			   of (pr:_) -> [pr]
			      []     -> []
	lexEsc _        =  []
lexLitChar (c:s)        =  [([c],s)]
lexLitChar ""           =  []

isOctDigit c  =  c >= '0' && c <= '7'
isHexDigit c  =  isDigit c || c >= 'A' && c <= 'F'
			   || c >= 'a' && c <= 'f'

lexmatch                   :: (Eq a) => [a] -> [a] -> ([a],[a])
lexmatch (x:xs) (y:ys) | x == y  =  lexmatch xs ys
lexmatch xs     ys               =  (xs,ys)

asciiTab = zip ['\NUL'..' ']
	   ["NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
	    "BS",  "HT",  "LF",  "VT",  "FF",  "CR",  "SO",  "SI",
	    "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
	    "CAN", "EM",  "SUB", "ESC", "FS",  "GS",  "RS",  "US",
	    "SP"]

readLitChar             :: ReadS Char
readLitChar ('\\':s)    =  readEsc s
 where
       readEsc ('a':s)  = [('\a',s)]
       readEsc ('b':s)  = [('\b',s)]
       readEsc ('f':s)  = [('\f',s)]
       readEsc ('n':s)  = [('\n',s)]
       readEsc ('r':s)  = [('\r',s)]
       readEsc ('t':s)  = [('\t',s)]
       readEsc ('v':s)  = [('\v',s)]
       readEsc ('\\':s) = [('\\',s)]
       readEsc ('"':s)  = [('"',s)]
       readEsc ('\'':s) = [('\'',s)]
       readEsc ('^':c:s) | c >= '@' && c <= '_'
			= [(chr (ord c - ord '@'), s)]
       readEsc s@(d:_) | isDigit d
			= [(chr n, t) | (n,t) <- readDec s]
       readEsc ('o':s)  = [(chr n, t) | (n,t) <- readOct s]
       readEsc ('x':s)  = [(chr n, t) | (n,t) <- readHex s]
       readEsc s@(c:_) | isUpper c
			= let table = ('\DEL',"DEL") : asciiTab
			  in case [(c,s') | (c, mne) <- table,
					    ([],s') <- [lexmatch mne s]]
			     of (pr:_) -> [pr]
				[]     -> []
       readEsc _        = []
readLitChar (c:s)       =  [(c,s)]

showLitChar               :: Char -> ShowS
showLitChar c | c > '\DEL' = showChar '\\' .
			     protectEsc isDigit (shows (ord c))
showLitChar '\DEL'         = showString "\\DEL"
showLitChar '\\'           = showString "\\\\"
showLitChar c | c >= ' '   = showChar c
showLitChar '\a'           = showString "\\a"
showLitChar '\b'           = showString "\\b"
showLitChar '\f'           = showString "\\f"
showLitChar '\n'           = showString "\\n"
showLitChar '\r'           = showString "\\r"
showLitChar '\t'           = showString "\\t"
showLitChar '\v'           = showString "\\v"
showLitChar '\SO'          = protectEsc ('H'==) (showString "\\SO")
showLitChar c              = showString ('\\' : snd (asciiTab!!ord c))

protectEsc p f             = f . cont
 where cont s@(c:_) | p c  = "\\&" ++ s
       cont s              = s

readDec, readOct, readHex :: Integral a => ReadS a
readDec = readInt 10 isDigit (\d -> ord d - ord '0')
readOct = readInt  8 isOctDigit (\d -> ord d - ord '0')
readHex = readInt 16 isHexDigit hex
	  where hex d = ord d -
			(if isDigit d
			   then ord '0'
			   else ord (if isUpper d then 'A' else 'a') - 10)

readInt :: Integral a => a -> (Char -> Bool) -> (Char -> Int) -> ReadS a
readInt radix isDig digToInt s =
    [(foldl1 (\n d -> n * radix + d) (map (fromIntegral . digToInt) ds), r)
	| (ds,r) <- nonnull isDig s ]

showInt    :: Integral a => a -> ShowS
showInt n r = let (n',d) = quotRem n 10
		  r'     = chr (ord '0' + fromIntegral d) : r
	      in  if n' == 0 then r' else showInt n' r'

readSigned:: Real a => ReadS a -> ReadS a
readSigned readPos = readParen False read'
		     where read' r  = read'' r ++
				      [(-x,t) | ("-",s) <- lex r,
						(x,t)   <- read'' s]
			   read'' r = [(n,s)  | (str,s) <- lex r,
						(n,"")  <- readPos str]

showSigned    :: Real a => (a -> ShowS) -> Int -> a -> ShowS
showSigned showPos p x = if x < 0 then showParen (p > 6)
						 (showChar '-' . showPos (-x))
				  else showPos x

readFloat     :: RealFloat a => ReadS a
readFloat r    = [(fromRational ((n%1)*10^^(k-d)),t) | (n,d,s) <- readFix r,
						       (k,t)   <- readExp s]
		 where readFix r = [(read (ds++ds'), length ds', t)
					| (ds,'.':s) <- lexDigits r,
					  (ds',t)    <- lexDigits s ]

		       readExp (e:s) | e `elem` "eE" = readExp' s
		       readExp s                     = [(0,s)]

		       readExp' ('-':s) = [(-k,t) | (k,t) <- readDec s]
		       readExp' ('+':s) = readDec s
		       readExp' s       = readDec s

showFloat     :: RealFloat a => a -> ShowS
showFloat x    = if x==0 then showString ("0." ++ take (m-1) (repeat '0'))
			 else if e >= m-1 || e < 0 then showSci else showFix
 where showFix   = showString whole . showChar '.' . showString frac
		   where (whole,frac) = splitAt (e+1) (show sig)
       showSci   = showChar d . showChar '.' . showString frac
			. showChar 'e' . shows e
		   where (d:frac) = show sig
       (m,sig,e) = if b == 10 then (w, s, n+w-1) else (m',sig',e')
       m'        = ceiling
		    (fromIntegral w * log (fromInteger b) / log 10 :: Double)
		   + 1
       (sig',e') = if      sig1 >= 10^m'     then (round (t/10), e1+1)
		   else if sig1 <  10^(m'-1) then (round (t*10), e1-1)
					     else (sig1, e1)
       sig1      = round t
       t         = s%1 * (b%1)^^n * 10^^(m'-e1-1)
       e1        = floor (logBase 10 x)
       (s,n)     = decodeFloat x
       b         = floatRadix x
       w         = floatDigits x

-- Monadic I/O: --------------------------------------------------------------

data IO a               -- builtin datatype of IO actions
data IOError            -- builtin datatype of IO error codes
type FilePath = String  -- file pathnames are represented by strings

primitive primbindIO   "rbindIO" :: IO a -> (a -> IO b) -> IO b
primitive primretIO    "runitIO" :: a -> IO a
primitive catch        "lbindIO" :: IO a -> (IOError -> IO a) -> IO a
primitive fail         "lunitIO" :: IOError -> IO ()
primitive putChar		 :: Char -> IO ()
primitive putStr		 :: String -> IO ()
primitive getChar   		 :: IO Char
primitive getContents 		 :: IO String
primitive writeFile   		 :: FilePath -> String -> IO ()
primitive appendFile  		 :: FilePath -> String -> IO ()
primitive readFile    		 :: FilePath -> IO String
primitive userError    		 :: String -> IOError

try       :: IO a -> IO (Either IOError a)
try p      = catch (p >>= (return . Right)) (return . Left)

print     :: Show a => a -> IO ()
print      = putStrLn . show

putStrLn  :: String -> IO ()
putStrLn s = do putStr s
		putChar '\n'

getLine   :: IO String
getLine    = do c <- getChar
		if c=='\n' then return ""
			   else do cs <- getLine
				   return (c:cs)

interact  :: (String -> String) -> IO ()
interact f = getContents >>= (putStr . f)

instance Functor IO where
    map f x = x >>= (return . f)

instance Monad IO where
    (>>=)  = primbindIO
    return = primretIO

-- Hooks for primitives: -----------------------------------------------------
-- Do not mess with these!

primCompAux      :: Ord a => a -> a -> Ordering -> Ordering
primCompAux x y o = case compare x y of EQ -> o; LT -> LT; GT -> GT

primPmInt        :: Num a => Int -> a -> Bool
primPmInt n x     = fromInt n == x

primPmInteger    :: Num a => Integer -> a -> Bool
primPmInteger n x = fromInteger n == x

primPmFlt        :: Fractional a => Double -> a -> Bool
primPmFlt n x     = fromDouble n == x

-- The following primitives are only needed if (n+k) patterns are enabled:
primPmNpk        :: Integral a => Int -> a -> Maybe a
primPmNpk n x     = if n'<=x then Just (x-n') else Nothing
		    where n' = fromInt n

primPmSub        :: Integral a => Int -> a -> a
primPmSub n x     = x - fromInt n

-- End of Hugs standard prelude ----------------------------------------------
