{-# LANGUAGE RankNTypes #-}

module Number where

import Data.Ratio
import GHC.Real
import Data.Function (on)
-- import Fraction

-- todo: derive Eq, Show, Ord, Bounded, etc
-- todo: derive Floating
-- todo: make NumQ -> NumZ if denominator is 1
-- todo: define operations for Rational's (newtype Fraction = Ratio Integer,
-- then make it a Floating instance)
-- update: Integral a => Ratio a  is a Num and Fractional, only need Floating

-- |Number is a wrapper for Haskell number types numbers using ADTs.
-- It supports integer, rational, and real number types.
-- It is polymorphic, as an instance of Num, Fractional and Floating.
-- It provides operations that keep the best precision possible, for
-- instance only degrading integers to rationals after division or
-- applying a binary function where the other argument is a fractional.
data Number = NumZ Integer | NumQ (Ratio Integer) | NumR Double

-- |Convert a Number to integer representation. Uses rounding when converting
-- from rationals and reals.
toZ :: Number -> Number
toZ (NumQ (x :% y)) = NumZ $ if y == 1 then x else round $ ((/) `on` fromIntegral) x y
toZ (NumR x) = NumZ $ round x
toZ (NumZ x) = NumZ x

-- |Convert a Number to rational representation. May experience rounding error
-- when converting from reals.
toQ :: Number -> Number
toQ (NumZ x) = NumQ $ x :% 1
toQ (NumR x) = NumQ $ toRational x
toQ (NumQ x) = NumQ x

-- |Convert a Number to real representation, may experience rounding error
toR :: Number -> Number
toR (NumZ x) = NumQ $ x :% 1
toR (NumQ x) = NumR $ fromRational x
toR (NumR x) = NumR x

-- func :: Real t => (t -> t -> t) -> (Number -> Number -> Number)
-- func f (NumR a) (NumR b) = NumR (realToDouble $ f a b)
-- -- func f a b = func f (toR a) (toR b)
-- x = func (*) (NumR 0) (NumR 0)

-- -- works
-- data X = A Double | B Int
-- func :: X -> X
-- func (A d) = A d
-- func (B i) = A $ realToDouble i

-- doesn't
-- lift a function from R->R to X->X
data A = X Double | Y Int
fmap :: Real t => (t -> t) -> A -> A
fmap f (X d) = X $ f d
fmap f (Y i) = X $ f (realToFrac i)


-- fma' :: X -> X)
-- fma' f (X d) = t

-- data X = A Double | B Int
-- func :: X -> X
-- func (A d) = A d
-- func (B i) = A $ realToDouble i
-- foobar :: Real t => (t -> t) -> X -> X
-- foobar f x@(B i) = foobar f (func x)
-- foobar f (A d) = A $ f d
-- 
realToDouble :: Real n => n -> Double
realToDouble i = fromRational $ toRational i

-- toDouble :: (forall n . Num n => n) -> Double; toDouble i = i
-- x = toDouble (5::Int) -- error, must be a polymorphic Num type

-- |Evaluate a *commutative* binary function of Number's with different
-- underlying types. Assumes 1) commutativity 2) f supports Integer, Rational,
-- Double. Makes sure the function recieves the same type
-- issue: Rational does not have most functions defined...
-- binaryFunction :: Num a => (a -> a -> a) -> (Number -> Number -> Number)
-- binaryFunction = (<&>) -- use shorter name because I'm lazy
-- (<&>) f (NumQ a) (NumQ b) = NumQ $ f a b
-- (<&>) f (NumZ a) (NumZ b) = NumZ $ f a b
-- (<&>) f (NumR a) (NumR b) = NumR $ f a b
-- -- for different type operands, we convert to same type Z->Q->R
-- (<&>) f (NumZ z) (NumQ q) = NumQ $ f (toQ z) q
-- (<&>) f (NumQ q) (NumR r) = NumR $ f (toR q) r
-- (<&>) f (NumZ z) (NumR r) = NumR $ f (toR z) r
-- -- exploit commutativity to reduce clutter
-- (<&>) f a b = (*) b a


-- instance Num Number where
--   -- abs :: Number -> Number
--   abs (NumQ (x :% y)) = NumQ $ (x * signum x) :% y -- todo
--   abs (NumZ  x)       = NumZ $ abs x
--   abs (NumR  x)       = NumR $ abs x
-- 
--   -- fromInteger :: Integer -> Number
--   fromInteger = NumZ
-- 
--   -- signum: sign of a number, -1 for negative, +1 for positive, +0 for 0
--   -- signum :: Number -> Number
--   signum (NumQ (x :% _)) = NumZ $ signum x -- todo
--   signum (NumZ  x)       = NumZ $ signum x
--   signum (NumR  x)       = NumZ $ round $ signum x
-- 
--   -- negate :: Number -> Number
--   negate (NumQ (x :% y)) = NumQ $ (negate x) :% y -- todo
--   negate (NumZ x) = NumZ $ signum x
--   negate (NumR x) = NumR $ signum x

  -- -- (*) :: Number -> Number -> Number
  -- (*) = binaryFunction (*)
  -- 
  -- -- (+) :: Number -> Number -> Number
  -- (+) = binaryFunction (+)

-- instance Fractional Number where
--   -- fromRational :: Ratio Integer -> Number
--   fromRational = NumQ
-- 
--   -- recip :: Number -> Number
--   recip (NumQ (x :% y)) = NumQ $ y % x
--   recip (NumZ  x)       = NumQ $ 1 :% x
--   recip (NumR  x)       = NumR $ 1.0 / x
-- 
--   -- (/) :: Number -> Number -> Number
--   -- we only overload division for a couple of special cases
--   (/) (NumR a) (NumR b) = if a == b then NumZ 1 else NumR $ a / b
--   (/) a b = a * recip b

-- instance Floating Number where
--   pi, exp, log, sin, cos, asin, acos, atan, sinh, cosh, asinh, acosh, atanh
