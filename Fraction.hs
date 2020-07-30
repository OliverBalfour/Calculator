
-- module Fraction where
-- 
-- import Data.Ratio
-- import GHC.Real
-- 
-- newtype Fraction = Ratio Integer
-- 
-- instance Num Fraction where
--   abs (x :% y) = (x * signum x) :% y
--   fromInteger 
--   signum (x :% y) = signum x
--   negate (x :% y) = negate x :% y
--   (*)
--   (+)

foo :: Real t => (t -> t) -> Double -> Double
foo f d = f d