> module Util where

> type R = Double

> infinity :: R
> infinity = 1 / 0

> class Power a where
>   pow :: a -> Integer -> a

> instance Power Float where
>   pow = (^)

> instance Power Double where
>   pow = (^)

> pair :: (a -> b, a -> c) -> a -> (b, c)
> pair (f, g) a = (f a, g a)

> cross :: (a -> b, c -> d) -> (a, c) -> (b, d)
> cross (f, g) (a, c) = (f a, g c)
