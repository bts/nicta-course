{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.State where

import Course.Core
import qualified Prelude as P
import Course.Optional
import Course.List
import Course.Functor
import Course.Apply
import Course.Applicative
import Course.Bind
import Course.Monad
import qualified Data.Set as S
import Data.Char

-- $setup
-- >>> import Test.QuickCheck.Function
-- >>> import Data.List(nub)
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> import Course.Core
-- >>> import Course.List
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- A `State` is a function from a state value `s` to (a produced value `a`, and a resulting state `s`).
newtype State s a =
  State {
    runState ::
      s
      -> (a, s)
  }

-- | Implement the `Functor` instance for `State s`.
-- >>> runState ((+1) <$> pure 0) 0
-- (1,0)
instance Functor (State s) where
  f <$> State rs = State $ \s0 -> let (a, s1) = rs s0
                                  in (f a, s1)

-- | Implement the `Apply` instance for `State s`.
-- >>> runState (pure (+1) <*> pure 0) 0
-- (1,0)
instance Apply (State s) where
  State rsl <*> State rsr = State $ \s0 -> let (f, s1) = rsl s0
                                               (v, s2) = rsr s1
                                           in (f v, s2)

-- | Implement the `Applicative` instance for `State s`.
-- >>> runState (pure 2) 0
-- (2,0)
instance Applicative (State s) where
  pure v = State $ \s -> (v, s)

-- | Implement the `Bind` instance for `State s`.
-- >>> runState ((const $ put 2) =<< put 1) 0
-- ((),2)
instance Bind (State s) where
  f =<< State rsr = State $ \s0 -> let (v0, s1) = rsr s0
                                       State rsl = f v0
                                   in rsl s1

instance Monad (State s) where

-- | Run the `State` seeded with `s` and retrieve the resulting state.
--
-- prop> \(Fun _ f) -> exec (State f) s == snd (runState (State f) s)
exec ::
  State s a
  -> s
  -> s
exec (State rs) s = snd $ rs s

-- | Run the `State` seeded with `s` and retrieve the resulting value.
--
-- prop> \(Fun _ f) -> eval (State f) s == fst (runState (State f) s)
eval ::
  State s a
  -> s
  -> a
eval (State rs) s = fst $ rs s

-- | A `State` where the state also distributes into the produced value.
--
-- >>> runState get 0
-- (0,0)
get ::
  State s s
get = State $ \s -> (s, s)

-- | A `State` where the resulting state is seeded with the given value.
--
-- >>> runState (put 1) 0
-- ((),1)
put ::
  s
  -> State s ()
put s = State $ \_ -> ((), s)

-- | Find the first element in a `List` that satisfies a given predicate.
-- It is possible that no element is found, hence an `Optional` result.
-- However, while performing the search, we sequence some `Monad` effect through.
--
-- Note the similarity of the type signature to List#find
-- where the effect appears in every return position:
--   find ::  (a ->   Bool) -> List a ->    Optional a
--   findM :: (a -> f Bool) -> List a -> f (Optional a)
--
-- >>> let p x = (\s -> (const $ pure (x == 'c')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Full 'c',3)
--
-- >>> let p x = (\s -> (const $ pure (x == 'i')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Empty,8)
findM ::
  Monad f =>
  (a -> f Bool)
  -> List a
  -> f (Optional a)
findM g = foldRight test (return Empty)
  where test a macc =
          g a >>= (\t -> if t
                         then return $ Full a
                         else macc)

-- | Find the first element in a `List` that repeats.
-- It is possible that no element repeats, hence an `Optional` result.
--
-- /Tip:/ Use `findM` and `State` with a @Data.Set#Set@.
--
-- prop> case firstRepeat xs of Empty -> let xs' = hlist xs in nub xs' == xs'; Full x -> length (filter (== x) xs) > 1
-- prop> case firstRepeat xs of Empty -> True; Full x -> let (l, (rx :. rs)) = span (/= x) xs in let (l2, r2) = span (/= x) rs in let l3 = hlist (l ++ (rx :. Nil) ++ l2) in nub l3 == l3
firstRepeat ::
  Ord a =>
  List a
  -> Optional a
firstRepeat as = eval findFirst S.empty
  where findFirst = findM mp as
        mp x = get >>= \s -> put (S.insert x s) >> get >>= \s' -> return $ s == s'

-- | Remove all duplicate elements in a `List`.
-- /Tip:/ Use `filtering` and `State` with a @Data.Set#Set@.
--
-- prop> firstRepeat (distinct xs) == Empty
--
-- prop> distinct xs == distinct (flatMap (\x -> x :. x :. Nil) xs)
distinct ::
  Ord a =>
  List a
  -> List a
distinct as = eval removeDupes S.empty
  where removeDupes = filtering p as
        p x = get >>= \s -> put (S.insert x s) >> return (not $ S.member x s)

-- | A happy number is a positive integer, where the sum of the squares of its digits eventually reaches 1 after repetition.
-- In contrast, a sad number (not a happy number) is where the sum of the square of its digits never reaches 1
-- because it results in a recurring sequence.
--
-- /Tip:/ Use `findM` with `State` and `produce`.
--
-- /Tip:/ Use `flatten` to write a @square@ function.
--
-- /Tip:/ Use library functions: @Optional#contains@, @Data.Char#digitToInt@.
--
-- >>> isHappy 4
-- False
--
-- >>> isHappy 7
-- True
--
-- >>> isHappy 42
-- False
--
-- >>> isHappy 44
-- True
square :: Num a => a -> a
square x = x * x

hStep :: Integer -> Integer
hStep x = foldLeft (+) 0 squaredDigits
  where squaredDigits = map (square . toInteger . digitToInt) $ listh $ show x

isHappy ::
  Integer
  -> Bool
isHappy x = contains 1 $ firstRepeat $ produce hStep x
