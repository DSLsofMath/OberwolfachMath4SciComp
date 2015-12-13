> module IN where

> import Util
> import LectureAD
> import IA

At each step of the Newton method we can find a dead end, if the
interval under examination does not contain a zero, or an interval
which might contain a zero and is so small that it does not warrant
further exploration, or an interval which might contain a zero and
does need to be explored further.  We collect the possible results
in the following datatype:

> data Result a  b  =  DE | P a | N b

> iNewtonStep f fext tol x  =  if diam x > tol
>                              then 
>                                 if empty x'
>                                    then DE
>                                    else N x'
>                              else
>                                 P x
>                          where
>                          mx   =  mid x
>                          fm   =  f mx
>                          fxs  =  fext (var x)
>                          fx'  =  fxs !! 1 
>                          x'   =  (toInterval mx - (toInterval fm) / fx') 
>                                  `meet` x

The fixed point of |Result a| is

> data Chain a = DeadEnd | Prize a | Next (Chain a)

An element of |Chain a| contains, burried under a pile of |Next|s,
either a |Prize| or a |DeadEnd|.

> foldChain f g e DeadEnd    =  e
> foldChain f g e (Prize a)  =  g a
> foldChain f g e (Next c)   =  f (foldChain f g e c)

Finding the prize or the disappointing |DeadEnd| is a fold:

> findPrize  =  foldChain id Just Nothing

Constructing a chain via an unfold of a |Result|-valued function:

> unfoldChain f x  =  case f x of
>                       DE    ->  DeadEnd
>                       P a   ->  Prize a
>                       N x'  ->  Next (unfoldChain f x')

Finally, in the Newton method we find the prize by applying 
the step procedure:

> iNewton f fext tol  =  findPrize . unfoldChain (iNewtonStep f fext tol)


> bar x = -2.001 + 3 * x - pow x 3

> testIN1 = iNewton bar bar (1e-15) (I (-3.0, -1.5))
> testIN2 = iNewton bar bar (1e-15) (I (1.5, 2.5))

