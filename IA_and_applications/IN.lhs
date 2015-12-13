> module IN where

Reference:   Tucker: ``Validated Numerics'', 2011
Implementation of the interval Newton method, section 5.1.3.  

> import Util
> import LectureAD
> import IA

> iNewton f fext tol x  =  if diam x > tol
>                             then 
>                                 if empty x'
>                                    then Nothing
>                                    else iNewton f fext tol x'
>                          else
>                             Just x
>                          where
>                          mx   =  mid x
>                          fm   =  f mx
>                          fxs  =  fext (var x)
>                          fx'  =  fxs !! 1 
>                          x'   =  (toInterval mx - (toInterval fm) / fx') 
>                                  `meet` x

> bar x = -2.001 + 3 * x - pow x 3

> testIN1 = iNewton bar bar (1e-15) (I (-3.0, -1.5))
> testIN2 = iNewton bar bar (1e-15) (I (1.5, 2.5))

