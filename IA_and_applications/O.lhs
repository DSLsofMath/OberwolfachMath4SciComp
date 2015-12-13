> module O where

> import Data.List
> import IA
> import Util

Reference: Tucker: ``Validated Numerics'', 2011.
Global optimization, section 5.2.1.

> opt' f fext tol (x, ub)
>   | ub < inf y = []
>   | rad y < tol && m < ub    =  [(inf y, x, m)]
>   | rad y < tol && m >= ub   =  [(inf y, x, ub)]
>   | rad y >= tol && m < ub   =  opt' f fext tol (leftHalf x, m) ++
>                                 opt' f fext tol (rightHalf x, m)
>   | rad y >= tol && m >= ub  =  opt' f fext tol (leftHalf x, ub) ++
>                                 opt' f fext tol (rightHalf x, ub)
>   where
>   y  =  fext x
>   m  =  f (mid x)

> prune xs = [x | (l, x, u) <- xs, l <= bestub]
>   where
>   bestub = minimum (map (\ (l, x, u) -> u) xs)

> opt f fext tol x  =  prune (opt' f fext tol (x, infinity))

> foo x   =  cos x

> test1   =  opt foo foo (2 ** (-10)) (I (-15, 15))
> test2   =  opt foo foo (2 ** (-40)) (I (-15, 15))
