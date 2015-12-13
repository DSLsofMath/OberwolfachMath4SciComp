> {-# LANGUAGE TypeSynonymInstances, FlexibleInstances, NPlusKPatterns #-}
> module OM where

> import Data.List
> import IA
> import Util

Implementation using a state-transformer monad.

> type State = R  -- our state consists only of the upper bound

> newtype ST a  =  ST (State -> (a, State)) -- state transformers
> (!)              ::  ST a -> State -> (a, State)
> (ST f) ! s        =  f s

> get              ::  ST State
> get               =  ST (pair (id, id))

> put              ::  State -> ST ()
> put s             =  ST (pair (const (), const s))

> instance Monad ST where
>   return x        =  ST (pair (const x, id))
>   f >>= g         =  ST h
>                      where
>                      h s = let (a, s') = f ! s in (g a) ! s'

> opt' f fext tol x  =  
>      do  ub <- get
>          if ub < inf y 
>             then return []
>             else do if m < ub
>                        then put m
>                        else return ()
>                     if rad y < tol
>                        then return [(inf y, x)]
>                        else do  xsl  <-  opt' f fext tol (leftHalf x)
>                                 xsr  <-  opt' f fext tol (rightHalf x)
>                                 return (xsl ++ xsr)
>      where
>      y  =  fext x
>      m  =  f (mid x)

> prune (xs, ub) = [x | (l, x) <- xs, l <= ub]

> opt f fext tol x = prune ((opt' f fext tol x) ! infinity)

> foo x   =  cos x

> test1   =  opt foo foo (2 ** (-10)) (I (-15, 15))
> test2   =  opt foo foo (2 ** (-40)) (I (-15, 15))

-------------------------------

> data Tree a = DeadEnd | Prize a | Branch (Tree a) (Tree a)

> foldTree f g e DeadEnd = e
> foldTree f g e (Prize a) = g a
> foldTree f g e (Branch tl tr) = f (foldTree f g e tl) (foldTree f g e tr)

> flatten = foldTree (++) (\ x -> [x]) []

> data Result a b = DE | P a | B b b

> munfoldTree f x = 
>            do fx <- f x
>               case fx of
>                  DE -> return DeadEnd
>                  P a  -> return (Prize a)
>                  B xl xr -> do tl <- munfoldTree f xl 
>                                tr <- munfoldTree f xr
>                                return (Branch tl tr)


> moptstep f fext tol x = 
>   do ub <- get
>      if ub < inf y 
>         then return DE
>         else do put (min m ub)
>                 if rad y < tol
>                    then return (P (inf y, x))
>                    else return (B (leftHalf x) (rightHalf x))
>   where
>   y  =  fext x
>   m  =  f (mid x)

> mopt f fext tol x = prune (cross (flatten, id) 
>       ((munfoldTree (moptstep f fext tol) x) ! infinity))

> test1'   =  mopt foo foo (2 ** (-10)) (I (-15, 15))
> test2'   =  mopt foo foo (2 ** (-40)) (I (-15, 15))


