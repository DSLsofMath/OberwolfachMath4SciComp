> module Z where

> import Util
> import IA

We re-write the |zeroTree| function constructing the tree of 
intervals as an unfold.

> data Tree a  =  DeadEnd | Prize a | Node (Tree a) a (Tree a) 
>                 deriving Show

The underlying functor for |Tree a| is

< F A X = 1 + A + X * A * X

> data F a x  =  DE | P a | N x a x

The unfold function is therefore:

> unfoldTree f x = case f x of
>                    DE         ->  DeadEnd
>                    P a        ->  Prize a
>                    N xl a xr  ->  Node (unfoldTree f xl)
>                                        a
>                                        (unfoldTree f xr)

The step performed at each step of the construction of
the tree is:

> zeroStep fext tol x  =  if (0 :: Interval) `isIn` x
>                            then if diam x <= tol
>                                    then P x
>                                    else N (leftHalf x)
>                                           x
>                                           (rightHalf x)
>                            else DE

Therefore, we can write |zeroTree| as:

> zeroTreeUnfold fext tol  =  unfoldTree (zeroStep fext tol)

> f x   =  sin x * (x - cos x)
> test  =  zeroTreeUnfold f 0.001 (I (-10, 10))

The |levels'| function used to list the levels of the constructed tree
had several inefficiencies which we can remove by applying |fold-map| 
fusion to the functions |concat . map cut| and |concat . map rest|.

<       concat . map cut 
< =  {- def. |concat| -}
<       foldr (++) [] . map cut
< =  {- |fold-map| fusion -}
<       foldr ((++) . cut) []

Similarly, |concat . map rest = foldr ((++) . rest) []

Moreover, since we now have two folds of the same list, we can use the
banana-split theorem to reduce them to only one fold.

Finally, we obtain:

> levels t = levels' [t]
>            where
>            cut DeadEnd = []
>            cut (Prize a) = [a]
>            cut (Node tl a tr) = [a]
>            rest DeadEnd = []
>            rest (Prize a) = []
>            rest (Node tl a tr) = [tl, tr]
>            levels' [] = []
>            levels' ts = let (heads, bodies) = foldr cut_n_rest ([], []) ts in
>                         heads : levels' bodies
>                         where
>                         cut_n_rest t (hs, rs) = (cut t ++ hs, rest t ++ rs)

> test' = levels test














