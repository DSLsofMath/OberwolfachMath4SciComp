> module Z where

> import Util
> import IA

Reference:  Tucker: ``Validated Numerics'', 2011.
Finding zeros by bisection, section 5.1.1.

> zeros  ::  (Interval -> Interval) -> R -> Interval -> [Interval]
> zeros fext tol x  =  if (0.0::R) `isIn` (fext x)
>                           then
>                             if (diam x <= tol)
>                                then [x]
>                                else zeros fext tol (leftHalf x) ++
>                                     zeros fext tol (rightHalf x)
>                           else []

> f x  = sin x * (x - cos x)
> test = zeros f 0.001 (I (-10, 10))

Constructing the tree of intervals examined:

> data Tree a = DeadEnd | Prize a | Node (Tree a) a (Tree a) 
>               deriving Show

> zeroTree fext tol x  =  if (0 :: Interval) `isIn` x
>                            then if diam x <= tol
>                                    then Prize x
>                                    else Node (zeroTree fext tol (leftHalf x))
>                                              x
>                                              (zeroTree fext tol (rightHalf x))
>                         else DeadEnd

> test' = zeroTree f 0.001 (I (-10, 10))

> levels t = levels' [t]
>            where
>            cut DeadEnd = []
>            cut (Prize a) = [a]
>            cut (Node tl a tr) = [a]
>            rest DeadEnd = []
>            rest (Prize a) = []
>            rest (Node tl a tr) = [tl, tr]
>            levels' [] = []
>            levels' ts = concat (map cut ts) : levels' (concat (map rest ts))

> test'' = levels test'


