-----------------------------------------------------------------------------
-- Standard Library: Monad operations
--
-- Suitable for use with Hugs 1.3.
-----------------------------------------------------------------------------

module Monad where

join             :: Monad m => m (m a) -> m a
join x            = x >>= id

apply            :: Monad m => (a -> m b) -> (m a -> m b)
apply f x         = x >>= f

(@@)             :: Monad m => (a -> m b) -> (c -> m a) -> (c -> m b)
f @@ g            = \x -> g x >>= f

mapAndUnzipL     :: Monad m => (a -> m (b,c)) -> [a] -> m ([b], [c])
mapAndUnzipL f xs = accumulateL (map f xs) >>= return . unzip

mapAndUnzipR     :: Monad m => (a -> m (b,c)) -> [a] -> m ([b], [c])
mapAndUnzipR f xs = accumulateR (map f xs) >>= return . unzip

accumulateL      :: Monad m => [m a] -> m [a]
accumulateL       = accumulate

accumulateR      :: Monad m => [m a] -> m [a]
accumulateR       = foldr mcons (return [])
                    where mcons p q = do xs<-q; x<-p; return (x:xs)

zipWithL         :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithL f xs ys  = accumulateL (zipWith f xs ys)

zipWithR         :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithR f xs ys  = accumulateR (zipWith f xs ys)

sequenceL        :: Monad m => [m a] -> m ()
sequenceL         = sequence

sequenceR        :: Monad m => [m a] -> m ()
sequenceR         = sequence . reverse

mapL             :: Monad m => (a -> m b) -> ([a] -> m [b])
mapL f []         = return []
mapL f (x:xs)     = do y<-f x; ys<-mapL f xs; return (y:ys)

mapR             :: Monad m => (a -> m b) -> ([a] -> m [b])
mapR f []         = return []
mapR f (x:xs)     = do ys<-mapR f xs; y<-f x; return (y:ys)

foldL            :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldL f a []      = return a
foldL f a (x:xs)  = do fax<-f a x; foldL f fax xs

foldR            :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldR f a []      = return a
foldR f a (x:xs)  = do y<-foldR f a xs; f x y

concatM          :: MonadPlus m => [m a] -> m a
concatM           = foldr (++) zero
 
unless           :: Monad m => Bool -> m () -> m ()
unless p s        = if p then done else s

when             :: Monad m => Bool -> m () -> m ()
when p s          = if p then s else done

-----------------------------------------------------------------------------
