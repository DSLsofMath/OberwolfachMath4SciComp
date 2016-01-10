> {-# LANGUAGE TypeSynonymInstances #-}

> module IA where

> import Util

> newtype Interval = I (R, R) deriving Eq

> toInterval :: R -> Interval
> toInterval x = I (x, x)

> instance Show Interval where
>     show (I (a, b)) = "[" ++ show a ++ ", " ++
>                       show b ++ "]"

> inf :: Interval -> R
> inf (I (a, b)) = a

> sup :: Interval -> R
> sup (I (a, b)) = b

> diam :: Interval -> R
> diam (I (a, b)) = b - a

> mid :: Interval -> R
> mid (I (a, b)) = (a + b) / 2.0

> rad :: Interval -> R
> rad x = 0.5 * diam x

> leftHalf :: Interval -> Interval
> leftHalf int = I (inf int, mid int)

> rightHalf :: Interval -> Interval
> rightHalf int = I (mid int, sup int)

> class Includable a where
>     isIn          :: a -> Interval -> Bool
>     isInInterior  :: a -> Interval -> Bool

> instance Includable R where
>     isIn         x (I (a, b)) = a <= x && x <= b
>     isInInterior x (I (a, b)) = isIn x (I (a, b)) && a /= b  -- TODO: is x==a OK? (I would have expected a<x && x<b)

> instance Includable Interval where
>     isIn         (I (a1, b1)) (I (a2, b2)) = a2 <= a1 && b1 <= b2
>     isInInterior (I (a1, b1)) (I (a2, b2)) =  a2 < a1 && b1 < b2

> mig :: Interval -> R
> mig (I (a, b)) = if (0::R) `isIn` I (a, b)
>                     then 0
>                     else min (abs a) (abs b)

> mag :: Interval -> R
> mag (I (a, b)) = max (abs a) (abs b)

> instance Ord Interval where
>     (<)  (I (a1, b1)) (I (a2, b2)) = b1 < a2
>     (<=) (I (a1, b1)) (I (a2, b2)) = b1 <= a2 || (I (a1, b1) == I (a2, b2))
>     (>)  (I (a1, b1)) (I (a2, b2)) = I (a2, b2) < I (a1, b1)
>     (>=) (I (a1, b1)) (I (a2, b2)) = I (a2, b2) <= I (a1, b1)

> instance Num Interval where
>     (+) (I (a1, b1)) (I (a2, b2))     = I (a1 + a2, b1 + b2)
>     negate (I (a1, b1)) = I (-b1, -a1)
>     (*) (I (a1, b1)) (I (a2, b2))     =
>         I (minimum [a1 * a2, a1 * b2, b1 * a2, b1 * b2],
>            maximum [a1 * a2, a1 * b2, b1 * a2, b1 * b2])
>     signum int
>         |  (0.0 :: R) `isIn` int      =  toInterval 0.0
>         |  int < toInterval 0.0       =  toInterval (-1.0)
>         |  int > toInterval 0.0       =  toInterval 1.0
>         |  otherwise                  =  error "Unexpected case in signum"
>     abs int                           =  I (mig int, mag int)
>     fromInteger                       =  toInterval . fromInteger

> instance Fractional Interval where
>     recip (I (al, ar))                =  I (1 / ar, 1 / al)  -- TODO: wrong if isIn 0 (I (al, ar))?
>     (/) a b                           =  a * (recip b)
>     fromRational                      =  toInterval . fromRational

> increasing :: (R -> R) -> Interval -> Interval
> increasing f (I (xl, xr)) = I (f xl, f xr)

> decreasing :: (R -> R) -> Interval -> Interval
> decreasing f (I (xl, xr)) = I (f xr, f xl)

> instance Floating Interval where
>     pi                    =  toInterval pi
>     exp                   =  increasing exp
>     log                   =  increasing log
>     sqrt                  =  increasing sqrt
>     (**)                  =  undefined  -- TODO: exercise
>     logBase               =  undefined  -- TODO: exercise
>     sin x @ (I (xl, xr))
>       |      splus && sminus  =  I (-1, 1)
>       |  not splus && sminus  =  I (-1, max (sin xl) (sin xr))
>       |  splus && not sminus  =  I (min (sin xl) (sin xr), 1)
>       |  otherwise            =  I (min (sin xl) (sin xr),
>                                     max (sin xl) (sin xr))
>       where
>     -- determine intersection with S+ = {2*k*pi + pi/2}
>     --                             S- = {2*k*pi - pi/2}
>     -- translate into intersection with Z
>       xplus  = (x - toInterval (pi / 2)) / toInterval (2 * pi)
>       splus  = floor (inf xplus) /= floor (sup xplus)
>       xminus = (x + toInterval (pi / 2)) / toInterval (2 * pi)
>       sminus = floor (inf xminus) /= floor (sup xminus)
>     cos x                 =  sin (x + toInterval (pi / 2))
>     tan                   =  increasing tan
>     asin                  =  undefined -- TODO: exercise
>     acos                  =  undefined -- TODO: exercise
>     atan                  =  increasing atan
>     sinh                  =  undefined -- TODO: exercise
>     cosh                  =  undefined -- TODO: exercise
>     tanh                  =  undefined -- TODO: exercise
>     asinh                 =  undefined -- TODO: exercise
>     acosh                 =  undefined -- TODO: exercise
>     atanh                 =  undefined -- TODO: exercise

> instance Power Interval where
>   pow x @ (I (xl, xr)) n
>       |  n > 0 && odd n       =  I (xl ^ n, xr ^ n)
>       |  n > 0 && even n      =  I ((mig x) ^ n, (mag x) ^ n)
>       |  n == 0               =  1
>       |  n < 0                =  pow (recip x) (-n)

> empty                        ::  Interval -> Bool
> empty (I (a, b))              =  a > b

> meet                         ::  Interval -> Interval -> Interval
> meet (I(a1, a2)) (I(b1, b2))  =
>   if  a2 < b1 || b2 < a1
>       then  I (1, 0) -- i.e., empty
>       else  (I (max a1 b1, min a2 b2))
