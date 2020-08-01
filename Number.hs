
module Number where

import Data.Ratio
import GHC.Real
import Data.Function (on)

-- |Number is a wrapper for Haskell number types numbers using ADTs.
-- It supports integer, rational, and real number types.
-- It is polymorphic, as an instance of Num, Fractional and Floating.
-- It provides operations that keep the best precision possible, for
-- instance only degrading integers to rationals after division or
-- applying a binary function where the other argument is a fractional.
-- As an added bonus you can use ** for all Numbers
data Number = NumZ Integer | NumQ (Ratio Integer) | NumR Double | NumFD { x :: Double, x' :: Double }

-- |Convert a Number to integer representation. Uses rounding when converting
-- from rationals and reals.
toZ :: Number -> Number
toZ (NumQ (x :% y)) = NumZ $ if y == 1 then x else round $ ((/) `on` fromIntegral) x y
toZ (NumR x) = NumZ $ round x
toZ y@(NumZ x) = y
toZ numFD = toZ $ toR numFD

-- |Convert a Number to rational representation. May experience rounding error
-- when converting from reals.
toQ :: Number -> Number
toQ (NumZ x) = NumQ $ x :% 1
toQ (NumR x) = NumQ $ toRational x
toQ y@(NumQ x) = y
toQ numFD = toQ $ toR numFD

-- |Convert a Number to real representation, may experience rounding error
toR :: Number -> Number
toR (NumZ x) = NumR $ fromRational (x :% 1)
toR (NumQ x) = NumR $ fromRational x
toR (NumR x) = NumR x
-- conversion to real always takes primal value x, not derivative x'
toR (NumFD x x') = NumR x'

-- |Convert a Number to a Forward Double for automatic differentiation
toFD :: Number -> Number
toFD y@(NumFD _ _) = y
toFD (NumR x) = NumFD x 1
toFD x = toFD $ toR x

toDisplay :: Number -> Number
toDisplay (NumFD _ x') = NumR x'
toDisplay a = a

toDouble :: Number -> Double
toDouble (NumR x) = x
toDouble x = toDouble . toR $ x

-- ported from CPython.Fraction.limit_denominator
-- https://github.com/python/cpython/blob/3.8/Lib/fractions.py#L227
limit_denominator :: Number -> Number -> Number
limit_denominator (NumZ lim) (NumQ frac) =
  NumQ $ limit (0, 1, 1, 0) lim frac
  where
    limit (p0, q0, p1, q1) lim frac@(n:%d) =
      let a = n `quot` d
          q2 = q0 + a * q1
      in if q2 > lim
        then
          let k = (lim - q0) `quot` q1
              bound1 = (p0 + k * p1) % (q0 + k * q1)
              bound2 = p1 % q1
          in if abs (bound2 - frac) <= abs (bound1 - frac)
            then bound2 else bound1
        else
          limit (p1, q1, p0 + a * p1, q2) lim (d :% (n - a * d))

limit_denominator a@(NumZ _) b = limit_denominator a (toQ b)
limit_denominator a b = limit_denominator (toZ a) b

-- |Convert a rational to a integer representation if denominator is one
maybeInt :: Rational -> Number
maybeInt (x :% 1) = NumZ x
maybeInt q = NumQ q

instance Show Number where
  show (NumZ x) = show x
  show (NumQ (x:%y)) = show x ++ "/" ++ show y
  show (NumR x) = show x
  show (NumFD x x') = show x ++ ", deriv: " ++ show x'

dblEq :: Double -> Double -> Bool
a `dblEq` b = abs (a - b) < 1e-6

instance Eq Number where
  -- (==) :: Number -> Number -> Bool
  -- note that 4:%2 /= 2:%1, 4%2 == 2%1
  (==) (NumQ (a:%b)) (NumQ (c:%d)) = (a%b) == (c%d)
  (==) (NumZ a) (NumZ b) = a == b
  (==) (NumR a) (NumR b) = a `dblEq` b
  (==) (NumFD a _) b = (NumR a) == b
  (==) z@(NumZ _) q@(NumQ _) = (toQ z) == q
  (==) q@(NumQ _) r@(NumR _) = (toR q) == r
  (==) z@(NumZ _) r@(NumR _) = (toR z) == r
  a == b = b == a

instance Ord Number where
  -- (<=) :: Number -> Number -> Bool
  (<=) (NumQ a) (NumQ b) = a <= b
  (<=) (NumZ a) (NumZ b) = a <= b
  (<=) (NumR a) (NumR b) = a <= b
  (<=) (NumFD a _) b = (NumR a) <= b
  (<=) z@(NumZ _) q@(NumQ _) = (toQ z) <= q
  (<=) q@(NumQ _) r@(NumR _) = (toR q) <= r
  (<=) z@(NumZ _) r@(NumR _) = (toR z) <= r
  a <= b = not (b <= a)

instance Num Number where
  -- abs :: Number -> Number
  abs (NumQ (x :% y)) = NumQ $ (x * signum x) :% y
  abs (NumZ  x)       = NumZ $ abs x
  abs (NumR  x)       = NumR $ abs x
  abs (NumFD x x') = NumFD (f x) (x' * df x)
    where f = abs; df = signum

  -- fromInteger :: Integer -> Number
  fromInteger = NumZ

  -- signum: sign of a number, -1 for negative, +1 for positive, +0 for 0
  -- signum :: Number -> Number
  signum (NumQ (x :% _)) = NumZ $ signum x -- todo
  signum (NumZ  x)       = NumZ $ signum x
  signum (NumR  x)       = NumZ $ round $ signum x
  signum (NumFD x x') = NumFD (f x) (x' * df x)
    where f = signum; df = const 0

  -- negate :: Number -> Number
  negate (NumQ (x :% y)) = NumQ $ (negate x) :% y
  negate (NumZ x) = NumZ $ negate x
  negate (NumR x) = NumR $ negate x
  negate (NumFD x x') = NumFD (f x) (x' * df x)
    where f = negate; df = const (-1)

  -- (*) :: Number -> Number -> Number
  (*) (NumQ a) (NumQ b) = maybeInt $ a * b
  (*) (NumZ a) (NumZ b) = NumZ $ a * b
  (*) (NumR a) (NumR b) = NumR $ a * b
  (*) z@(NumZ _) q@(NumQ _) = (toQ z) * q
  (*) q@(NumQ _) r@(NumR _) = (toR q) * r
  (*) z@(NumZ _) r@(NumR _) = (toR z) * r
  (*) (NumFD a a') (NumFD b b') = NumFD (a*b) (a*b'+b*a')
  (*) (NumFD a a') (NumR b) = NumFD (a*b) (a'*b)
  (*) a@(NumFD _ _) b = a * (toR b)
  a * b = b * a

  -- (+) :: Number -> Number -> Number
  (+) (NumQ a) (NumQ b) = maybeInt $ a + b
  (+) (NumZ a) (NumZ b) = NumZ $ a + b
  (+) (NumR a) (NumR b) = NumR $ a + b
  (+) z@(NumZ _) q@(NumQ _) = (toQ z) + q
  (+) q@(NumQ _) r@(NumR _) = (toR q) + r
  (+) z@(NumZ _) r@(NumR _) = (toR z) + r
  (+) (NumFD a a') (NumFD b b') = NumFD (a+b) (a'+b')
  (+) (NumFD a a') (NumR b) = NumFD (a+b) a'
  (+) a@(NumFD _ _) b = a + (toR b)
  a + b = b + a

instance Fractional Number where
  -- fromRational :: Ratio Integer -> Number
  fromRational = NumQ

  -- recip :: Number -> Number
  recip (NumQ (x :% y)) = maybeInt $ y % x
  recip (NumZ  x)       = NumQ $ 1 :% x
  recip (NumR  x)       = NumR $ 1.0 / x
  recip (NumFD x x')    = NumFD (1.0 / x) ((-x') / x**2)

  -- (/) :: Number -> Number -> Number
  -- we only overload division for a couple of special cases
  (/) (NumR a) (NumR b) = if a == b then NumZ 1 else NumR $ a / b
  (/) a b = a * recip b

instance Floating Number where
  -- pi :: Number
  pi = NumR pi

  -- Overload (**) to use (^), (^^) where possible for efficiency
  (**) (NumQ a) (NumZ b) = NumQ $ a ^^ b
  (**) (NumR a) (NumZ b) = NumR $ a ^^ b
  (**) (NumZ a) (NumZ b) = if b < 0
    then NumQ $ (a:%1) ^^ b
    else NumZ $ a^b
  (**) a b = exp (log a * b)

  -- All unary functions have the same structure
  -- We cannot extract these to functions so, eg exp = wrapFunction exp
  -- because the type system complains about rigid type variables if we
  -- create a function :: Floating t => (t -> t) -> Number -> Number
  -- that internally applies said function

  -- unary_function :: Number -> Number
  -- unary_function (NumFD x x') = NumFD (unary_function x, derivative)
  -- unary_function (NumR r) = NumR $ unary_function r
  -- unary_function x = unary_function (toR x)

  exp (NumFD x x') = NumFD (exp x) (x' * exp x)
  exp (NumR r) = NumR $ exp r
  exp x = exp (toR x)

  log (NumFD x x') = NumFD (log x) (x' / x)
  log (NumR r) = NumR $ log r
  log x = log (toR x)

  sin (NumFD x x') = NumFD (sin x) (x' * cos x)
  sin (NumR r) = NumR $ sin r
  sin x = sin (toR x)

  cos (NumFD x x') = NumFD (sin x) (x' * (negate . sin) x)
  cos (NumR r) = NumR $ cos r
  cos x = cos (toR x)

  asin (NumFD x x') = NumFD (asin x) (x' / sqrt (1 - x**2))
  asin (NumR r) = NumR $ asin r
  asin x = asin (toR x)

  acos (NumFD x x') = NumFD (acos x) ((-x') / sqrt (1 - x**2))
  acos (NumR r) = NumR $ acos r
  acos x = acos (toR x)

  atan (NumFD x x') = NumFD (atan x) (x' / (1 + x**2))
  atan (NumR r) = NumR $ atan r
  atan x = atan (toR x)

  sinh (NumFD x x') = NumFD (sinh x) (x' * cosh x)
  sinh (NumR r) = NumR $ sinh r
  sinh x = sinh (toR x)

  cosh (NumFD x x') = NumFD (cosh x) (x' * sinh x)
  cosh (NumR r) = NumR $ cosh r
  cosh x = cosh (toR x)

  asinh (NumFD x x') = NumFD (asinh x) (x' / sqrt (1 + x**2))
  asinh (NumR r) = NumR $ asinh r
  asinh x = asinh (toR x)

  acosh (NumFD x x') = NumFD (acosh x) (x' / sqrt (1 - x**2))
  acosh (NumR r) = NumR $ acosh r
  acosh x = acosh (toR x)

  atanh (NumFD x x') = NumFD (atanh x) (x' / (1 - x**2))
  atanh (NumR r) = NumR $ atanh r
  atanh x = atanh (toR x)

-- permutations, combinations, factorial
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- note: factorial rounds for non-integers instead of using the gamma function
numFactorial :: Number -> Number
numFactorial (NumZ n) = if n >= 0 then NumZ (factorial n) else 0
numFactorial a = numFactorial $ toZ a

perms :: Number -> Number -> Number
perms n r = numFactorial n / numFactorial (n - r)

choose :: Number -> Number -> Number
choose n r = perms n r / numFactorial r

-- gcd rounds non-integers
numGCD :: Number -> Number -> Number
numGCD (NumZ a) (NumZ b) = NumZ $ gcd a b
numGCD a b = numGCD (toZ a) (toZ b)
