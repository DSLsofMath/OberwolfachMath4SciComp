> module K where

> import Util
> import AD
> import IA

The possible results at each step in the Krawczyk method are: either
the interval under consideration cannot contain a zero, or we can
check whether it is small enough and contains a unique zero, or needs
further exploration, in which case we divide it and obtain two new
intervals.  The datatype summarizing these possibilities is:

> data Results a b  =  NZ | M a | U a | N b b

The |krawczyk| step can then be written as:

> krawczykStep f fext tol x  =  if empty kx
>                                  then NZ
>                                  else if diam kx < tol
>                                          then if kx `isInInterior` x
>                                                  then U kx
>                                                  else M kx
>                                          else N (leftHalf kx) (rightHalf kx)
>                               where
>                               m    =  mid x
>                               fms  =  f (var m)
>                               fm   =  fms !! 0
>                               fm'  =  fms !! 1
>                               fxs  =  fext (var x)
>                               fx'  =  fxs !! 1 
>                               r    =  rad x
>                               rr   =  I (-r, r)
>                               kx   =  (toInterval (m - fm / fm') -
>                                       (1 - fx' / (toInterval fm')) * rr)
>                                       `meet` x

The fixed point of the functor |Result a| is

> data Tree a  =  NoZero | MaybeZero a | UniqueZero a | Node (Tree a) (Tree a)
>                 deriving Show

> foldTree f g h e NoZero          =  e
> foldTree f g h e (MaybeZero a)   =  h a
> foldTree f g h e (UniqueZero a)  =  g a
> foldTree f g h e (Node tl tr)    =  f (foldTree f g h e tl) (foldTree f g h e tr)

> unfoldTree f x  =  case f x of
>                      NZ       ->  NoZero
>                      M a      ->  MaybeZero  a
>                      U a      ->  UniqueZero a
>                      N xl xr  ->  Node (unfoldTree f xl) (unfoldTree f xr)

Collecting the possible solutions from a tree is a fold:

> flatten  =  foldTree (++) (wrap . UniqueZero) (wrap . MaybeZero) [] where wrap x = [x]

Finally, the Krawczyk method is a flattening of the tree of
possible solutions constructing by unfolding the Krawczyk step:

> krawczyk f fext tol = flatten . unfoldTree (krawczykStep f fext tol)

> bar x  =  sin x * (x - cos x)
> test   =  krawczyk bar bar 0.0001 (I (-10, 10.000))