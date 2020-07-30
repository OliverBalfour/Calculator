
module Number where

import Data.Ratio
import GHC.Real
import Data.Function (on)

-- todo: derive Eq, Show, Ord, Bounded, etc

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

-- |Convert a rational to a integer representation if denominator is zero
maybeInt :: Rational -> Number
maybeInt (x :% 1) = NumZ x
maybeInt q = NumQ q

instance Num Number where
  -- abs :: Number -> Number
  abs (NumQ (x :% y)) = NumQ $ (x * signum x) :% y
  abs (NumZ  x)       = NumZ $ abs x
  abs (NumR  x)       = NumR $ abs x

  -- fromInteger :: Integer -> Number
  fromInteger = NumZ

  -- signum: sign of a number, -1 for negative, +1 for positive, +0 for 0
  -- signum :: Number -> Number
  signum (NumQ (x :% _)) = NumZ $ signum x -- todo
  signum (NumZ  x)       = NumZ $ signum x
  signum (NumR  x)       = NumZ $ round $ signum x

  -- negate :: Number -> Number
  negate (NumQ (x :% y)) = NumQ $ (negate x) :% y
  negate (NumZ x) = NumZ $ signum x
  negate (NumR x) = NumR $ signum x

  -- (*) :: Number -> Number -> Number
  (*) (NumQ a) (NumQ b) = maybeInt $ a * b
  (*) (NumZ a) (NumZ b) = NumZ $ a * b
  (*) (NumR a) (NumR b) = NumR $ a * b
  (*) z@(NumZ _) q@(NumQ _) = (toQ z) * q
  (*) q@(NumQ _) r@(NumR _) = (toR q) * r
  (*) z@(NumZ _) r@(NumR _) = (toR z) * r
  (*) a b = (*) b a

  -- (+) :: Number -> Number -> Number
  (+) (NumQ a) (NumQ b) = maybeInt $ a + b
  (+) (NumZ a) (NumZ b) = NumZ $ a + b
  (+) (NumR a) (NumR b) = NumR $ a + b
  (+) z@(NumZ _) q@(NumQ _) = (toQ z) + q
  (+) q@(NumQ _) r@(NumR _) = (toR q) + r
  (+) z@(NumZ _) r@(NumR _) = (toR z) + r
  (+) a b = (+) b a

instance Fractional Number where
  -- fromRational :: Ratio Integer -> Number
  fromRational = NumQ

  -- recip :: Number -> Number
  recip (NumQ (x :% y)) = maybeInt $ y % x
  recip (NumZ  x)       = NumQ $ 1 :% x
  recip (NumR  x)       = NumR $ 1.0 / x

  -- (/) :: Number -> Number -> Number
  -- we only overload division for a couple of special cases
  (/) (NumR a) (NumR b) = if a == b then NumZ 1 else NumR $ a / b
  (/) a b = a * recip b

instance Floating Number where
  -- pi :: Number
  pi = NumR pi

  -- All unary functions have the same structure
  -- We cannot extract these to functions so, eg exp = wrapFunction exp
  -- because the type system complains about rigid type variables if we
  -- create a function :: Floating t => (t -> t) -> Number -> Number
  -- that internally applies said function

  -- unary_function :: Number -> Number
  -- unary_function (NumR r) = NumR $ unary_function r
  -- unary_function x@(NumZ _) = unary_function (toR x)
  -- unary_function x@(NumQ _) = unary_function (toR x)

  -- The following snippet generates the below code:
  -- import qualified Data.Text as T
  -- main = putStrLn (T.unpack generated)
  -- generated = T.unlines $ map (\fn -> T.replace (T.pack "*") fn template) fns where
  --   fns = (T.split (==',') (T.pack "exp,log,sin,cos,asin,acos,atan,sinh,cosh,asinh,acosh,atanh"))
  --   template = (T.pack"* (NumR r) = NumR $ * r\n\
  -- \* x@(NumZ _) = * (toR x)\n\
  -- \* x@(NumQ _) = * (toR x)")

  exp (NumR r) = NumR $ exp r
  exp x@(NumZ _) = exp (toR x)
  exp x@(NumQ _) = exp (toR x)
  log (NumR r) = NumR $ log r
  log x@(NumZ _) = log (toR x)
  log x@(NumQ _) = log (toR x)
  sin (NumR r) = NumR $ sin r
  sin x@(NumZ _) = sin (toR x)
  sin x@(NumQ _) = sin (toR x)
  cos (NumR r) = NumR $ cos r
  cos x@(NumZ _) = cos (toR x)
  cos x@(NumQ _) = cos (toR x)
  asin (NumR r) = NumR $ asin r
  asin x@(NumZ _) = asin (toR x)
  asin x@(NumQ _) = asin (toR x)
  acos (NumR r) = NumR $ acos r
  acos x@(NumZ _) = acos (toR x)
  acos x@(NumQ _) = acos (toR x)
  atan (NumR r) = NumR $ atan r
  atan x@(NumZ _) = atan (toR x)
  atan x@(NumQ _) = atan (toR x)
  sinh (NumR r) = NumR $ sinh r
  sinh x@(NumZ _) = sinh (toR x)
  sinh x@(NumQ _) = sinh (toR x)
  cosh (NumR r) = NumR $ cosh r
  cosh x@(NumZ _) = cosh (toR x)
  cosh x@(NumQ _) = cosh (toR x)
  asinh (NumR r) = NumR $ asinh r
  asinh x@(NumZ _) = asinh (toR x)
  asinh x@(NumQ _) = asinh (toR x)
  acosh (NumR r) = NumR $ acosh r
  acosh x@(NumZ _) = acosh (toR x)
  acosh x@(NumQ _) = acosh (toR x)
  atanh (NumR r) = NumR $ atanh r
  atanh x@(NumZ _) = atanh (toR x)
  atanh x@(NumQ _) = atanh (toR x)
