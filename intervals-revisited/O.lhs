> module O where

> import Data.List
> import IA
> import Util

At every step of the optimization, we have the following possible
results: either the interval we are examining has no minimum, because
the lower bound of its image is above the current upper bound, or it
might contain a minimum and be small, in which case we save it together
with the lower bound of its image and the current upper bound, or it
might contain a minimum and we need to explore it further, in which 
case we return two new intervals, each paired with the current upper
bound.  The |Result| type is therefore:

> data Result a b = DE | P a | B b b

and a step of the optimization can be written as

> optStep f fext tol (x, ub)
>   | ub < inf y    =  DE
>   | rad y < tol   =  P (inf y, x, ub')
>   | rad y >= tol  =   B (leftHalf x, ub')
>                         (rightHalf x, ub')
>   where
>   y  =  fext x
>   m  =  f (mid x)
>   ub' = min m ub

The fixed point of the functor |Result a| is

> data Tree a = DeadEnd | Prize a | Branch (Tree a) (Tree a)

with fold and unfold functions:

> foldTree f g e DeadEnd = e
> foldTree f g e (Prize a) = g a
> foldTree f g e (Branch tl tr) = f (foldTree f g e tl) (foldTree f g e tr)

> unfoldTree f x = case f x of
>                  DE -> DeadEnd
>                  P a  -> Prize a
>                  B xl xr -> Branch (unfoldTree f xl) (unfoldTree f xr)

The collection of possible minima-containing intervals is a fold:

> flatten = foldTree (++) (\ x -> [x]) []

The resulting collection needs to be pruned to eliminate spurious
intervals:

> prune xs = [x | (l, x, u) <- xs, l <= bestub]
>   where
>   bestub = minimum (map (\ (l, x, u) -> u) xs)

Finally, the optimization can be written as the result of pruning the
collection of possible solutions constructed by |optStep|:

> opt f fext tol x = 
>   prune (flatten (unfoldTree (optStep f fext tol) (x, infinity)))



> foo x   =  cos x

> test1   =  opt foo foo (2 ** (-10)) (I (-15, 15))
> test2   =  opt foo foo (2 ** (-40)) (I (-15, 15))

f