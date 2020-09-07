-----------------------------------------------------------------------------
-- Standard Library: Operations on the Maybe datatype
--
-- Warning: the contents of this file are based on a preliminary version of
-- the Haskell 1.3 libraries, and are likely to change in future revisions.
--
-- Suitable for use with Hugs 1.3.
-----------------------------------------------------------------------------

module Maybe where

exists            :: Maybe a -> Bool
exists             = maybe False (const True)

the               :: Maybe a -> a
the                = maybe (error "Maybe.the: Nothing") id

theExists         :: Maybe a -> (a, Bool)
theExists          = maybe (error "Maybe.theExists: Nothing", False)
                           (\a -> (a, True))

fromMaybe         :: a -> Maybe a -> a
fromMaybe d        = maybe d id

maybeToList       :: Maybe a -> [a]
maybeToList        = maybe [] (\ x -> [x])

listToMaybe       :: [a] -> Maybe a
listToMaybe []     = Nothing
listToMaybe (a:_)  = Just a

findMaybe         :: (a -> Bool) -> [a] -> Maybe a
findMaybe p        = listToMaybe . filter p

catMaybes         :: [Maybe a] -> [a]
catMaybes xs       = [ x | (Just x) <- xs ]

mapMaybe          :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f         = catMaybes . map f

-----------------------------------------------------------------------------
