> module K where

> import Util
> import AD
> import IA

Reference:   Tucker: ``Validated Numerics'', 2011.
The Krawczyk method, section 5.1.5.

> data ZResults a = NoZero | MaybeZero a | UniqueZero a deriving Show

> krawczyk f fext tol x  =  if empty kx
>                              then []
>                           else if diam kx < tol
>                                   then if kx `isInInterior` x
>                                           then [UniqueZero kx]
>                                           else [MaybeZero kx]
>                                   else krawczyk f fext tol (leftHalf kx) ++
>                                        krawczyk f fext tol (rightHalf kx)
>                           where
>                           m    =  mid x
>                           fms  =  f (var m)
>                           fm   =  fms !! 0
>                           fm'  =  fms !! 1
>                           fxs  =  fext (var x)
>                           fx'  =  fxs !! 1 
>                           r    =  rad x
>                           rr   =  I (-r, r)
>                           kx   =  (toInterval (m - fm / fm') -
>                                       (1 - fx' / (toInterval fm')) * rr)
>                                   `meet` x

> bar x  =  sin x * (x - cos x)

> test   =  krawczyk bar bar 0.0001 (I (-10, 10.000))