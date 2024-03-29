{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Extend where

import Course.Core
import Course.Id
import Course.List
import Course.Optional
import Course.Functor

class Functor f => Extend f where
  -- Pronounced, extend.
  (<<=) ::
    (f a -> b)
    -> f a
    -> f b

infixr 1 <<=

-- | Implement the @Extend@ instance for @Id@.
--
-- >>> id <<= Id 7
-- Id (Id 7)
instance Extend Id where
  f <<= ma = Id $ f ma

-- | Implement the @Extend@ instance for @List@.
--
-- >>> length <<= ('a' :. 'b' :. 'c' :. Nil)
-- [3,2,1]
--
-- >>> id <<= (1 :. 2 :. 3 :. 4 :. Nil)
-- [[1,2,3,4],[2,3,4],[3,4],[4]]
--
-- >>> reverse <<= ((1 :. 2 :. 3 :. Nil) :. (4 :. 5 :. 6 :. Nil) :. Nil)
-- [[[4,5,6],[1,2,3]],[[4,5,6]]]
instance Extend List where
  _ <<= Nil = Nil
  f <<= xs@(_:.rest) = f xs :. (f <<= rest)

-- | Implement the @Extend@ instance for @Optional@.
--
-- >>> id <<= (Full 7)
-- Full (Full 7)
--
-- >>> id <<= Empty
-- Empty
instance Extend Optional where
  _ <<= Empty = Empty
  f <<= ma = Full $ f ma

-- | Duplicate the functor using extension.
--
-- >>> cojoin (Id 7)
-- Id (Id 7)
--
-- >>> cojoin (1 :. 2 :. 3 :. 4 :. Nil)
-- [[1,2,3,4],[2,3,4],[3,4],[4]]
--
-- >>> cojoin (Full 7)
-- Full (Full 7)
--
-- >>> cojoin Empty
-- Empty
cojoin ::
  Extend f =>
  f a
  -> f (f a)
cojoin ma = id <<= ma
