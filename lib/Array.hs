-----------------------------------------------------------------------------
-- Standard Library: Array operations
--
-- Suitable for use with Hugs 1.3.
-----------------------------------------------------------------------------

module Array where

infixl 9  !, //

data Array a b -- Arrays are implemented as a primitive type

array          :: Ix a => (a,a) -> [(a,b)] -> Array a b
listArray      :: Ix a => (a,a) -> [b] -> Array a b
(!)	       :: Ix a => Array a b -> a -> b
bounds         :: Ix a => Array a b -> (a,a)
indices        :: Ix a => Array a b -> [a]
elems          :: Ix a => Array a b -> [b]
assocs	       :: Ix a => Array a b -> [(a,b)]
accumArray     :: Ix a => (b -> c -> b) -> b -> (a,a) -> [(a,c)] -> Array a b
(//)           :: Ix a => Array a b -> [(a,b)] -> Array a b
accum          :: Ix a => (b -> c -> b) -> Array a b -> [(a,c)] -> Array a b
amap	       :: Ix a => (b -> c) -> Array a b -> Array a c
ixmap	       :: (Ix a, Ix b) => (a,a) -> (a -> b) -> Array b c -> Array a c

primitive primArray  :: [(a,b)] -> (a,a) -> (a -> Int) -> Array a b
primitive primUpdate :: [(a,b)] -> Array a b -> (a -> Int) -> Array a b
primitive primAccum
    :: [(a,c)] -> Array a b -> (b -> c -> b) -> (a -> Int) -> Array a b
primitive primAccumArray
    :: [(a,c)] -> (a,a) -> b -> (b -> c -> b) -> (a -> Int) -> Array a b
primitive primSubscript
    :: ((a,a) -> a -> Int) -> Array a b -> a -> b

primitive primBounds :: Array a b -> (a,a)
primitive primElems  :: Array a b -> [b]
primitive primAmap   :: (b -> c) -> Array a b -> Array a c

array bounds assocs = primArray assocs bounds (index bounds)
listArray b vs      = array b (zip (range b) vs)
(!)                 = primSubscript index
bounds              = primBounds
indices	      = range . bounds
elems               = primElems
assocs a            = zip (indices a) (elems a)
accumArray f z b as = primAccumArray as b z f (index b)
a // as             = primUpdate as a (index (bounds a))
accum f a as        = primAccum as a f (index (bounds a))
amap                = primAmap
ixmap b f a         = array b [ (i, a ! f i) | i <- range b ]

instance (Ix a, Eq b) => Eq (Array a b) where
    a == a'   =   assocs a == assocs a'

instance (Ix a, Ord b) => Ord (Array a b) where
    a <= a'   =   assocs a <= assocs a'

instance (Ix a, Show a, Show b) => Show (Array a b) where
    showsPrec p a = showParen (p > 9) (
                       showString "array " .
                       shows (bounds a)    .
		       showChar ' '        .
                       shows (assocs a))

-----------------------------------------------------------------------------
