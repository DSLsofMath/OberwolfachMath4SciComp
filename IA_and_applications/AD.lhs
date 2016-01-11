> {-# LANGUAGE TypeSynonymInstances, FlexibleInstances, NPlusKPatterns #-}

> module AD where

> import Util

References:

  * Pavlovici and Escardo: ``Calculus in codinductive form'', 1998
  * McIlroy: ``Power series, power serious'', 1999
  * Tucker: ``Validated Numerics'', 2011

In many application, e.g. Newton's method, many optimization methods,
etc., we need to compute the values of the derivatives of a real
function.  There are many ways of doing this, e.g.:

> type FD a = (a, a)

A value |(f, f') :: FD a| is supposed to represent the value of a
function |F| and its derivative |F'| at a fixed point |x0|.  In
particular, if |F| is a constant function, say |F x = c|, then
|(f, f') = (c, 0)| irrespective of |x0|, and if |F| is the identity
function, then |(f, f') = (x0, 1)| at |x0|.  These examples
represent a kind of embedding of values in the datatype of pairs.

> conFD :: Num a => a -> FD a
> conFD a = (a, 0)
> varFD :: Num a => a -> FD a
> varFD a = (a, 1)

Having fixed |x0|, we can perform arithmetic operations on |FD a|:

> instance Num a => Num (FD a) where
>   (+) (f, f') (g, g')   =  (f + g, f' + g')
>   negate (f, f')        =  (-f, -f')
>   (*) (f, f') (g, g')   =  (f * g, f * g' + f' * g)
>   signum (f, f')        =  (signum f, 0)
>   abs (f, f')           =  (abs f, signum f * f')
>   fromInteger n         =  (fromInteger n, 0)

> instance Fractional a => Fractional (FD a) where
>   (/) (f, f') (g, g') =  let fdg = f / g in
>                          (fdg, (f' - fdg * g') / g)
>   fromRational r      =  (fromRational r, 0)

One might expect elementary functions such as |sin| to be treated
similarly to |conFD| or |varFD| above, but that would be a bad idea.
What we need is to extend these functions to operate on values of
type |FD a|.

> instance Floating a => Floating (FD a) where
>   pi                  =  conFD pi
>   exp (f, f')         =  let e = exp f in (e, f' * e)
>   log (f, f')         =  (log f, f' / f)
>   sqrt (f, f')        =  let sqrtf = sqrt f in
>                          (sqrtf, -0.5 * f' * (1 / sqrtf))
>   (**)                =  undefined -- TODO: exercise
>   sin (f, f')         =  (sin f, (cos f) * f')
>   cos (f, f')         =  (cos f, (-sin f) * f')
>   tan                 =  undefined -- TODO: exercise
>   asin                =  undefined -- TODO: exercise
>   acos                =  undefined -- TODO: exercise
>   atan                =  undefined -- TODO: exercise
>   sinh                =  undefined -- TODO: exercise
>   cosh                =  undefined -- TODO: exercise
>   tanh                =  undefined -- TODO: exercise
>   asinh               =  undefined -- TODO: exercise
>   acosh               =  undefined -- TODO: exercise
>   atanh               =  undefined -- TODO: exercise


> newtonFD :: (Ord a, Floating a) => (FD a -> FD a) -> a -> a -> a
> newtonFD f tol x
>   | abs (x - x') < tol  =  x'
>   | otherwise           =  newtonFD f tol x'
>   where (fx, fx')  =  f (varFD x)
>         x'         =  x - (fx / fx')

> foo :: Floating a => a -> a
> foo x = sin (exp x + 1)

> test1 :: R
> test1 = newtonFD foo (1e-10) 0

This works very well, and can be applied with no changes to any
suitable type (instance of |Floating|), including intervals.  But it has
a disadvantage: we cannot use this machinery to compute higher-order
derivatives, such as needed, e.g., for some optimization methods (we
need the second derivative to check critical points).

We could copy the ideas and work with triples, etc.  A more interesting
approach, however, is to work with \emph{infinite lists}, also known
as streams.

> data Stream a = S a (Stream a) deriving Show

declares the elements of type |Stream a| to be of the form

< a0 `S` a1 `S` a2 ...

|Stream a| is isomorphic to |Nat -> a|.  In Haskell, the datatype of
lists contains also infinite lists, so we can use the same notation for
streams.

> foldStream :: (a -> x -> x) -> [a] -> x
> foldStream f (a : as) = f a (foldStream f as)

> unfoldStream :: (x -> (a, x)) -> x -> [a]
> unfoldStream f x = let (a, x') = f x in
>                    a : (unfoldStream f x')

We can use streams to represent the values of the Taylor coefficients of
a |Cinf| function |f| at a given |x0|.  Thus, if

< fs = [f0, f1, f2, ..., fi, ...]

then

< fi = (fderivi x0) / i!

The essential property of Taylor series can be expressed as

< f x = Sumk0inf fk * (x - x0) ^ k

or, in Horner form:

< f x = f0 + (x - x0) * (f1 + (x - x1) * ( ...

> eval x0 x = foldStream h
>   where h fk e = fk + (x - x0) * e

> evalTo n delta = foldr1 (\ fk s -> fk + delta * s) . take n

Since if we have the Taylor expansions of |f| and |g| at |x0| we can
compute the Taylor expansions of |f + g|, |-f|, |f * g|, we can install
infinite series as instance of |Num|.  The more interesting of these
operations is |(*)|.  The point of view usually taken in textbooks
(including Tucker's "Validated Numerics") is that of |Nat -> a|, which
encourages the following sort of calculation:

< eval x0 x (fs * gs) = eval x0 x fs * eval x0 x gs


% format Sumi0k  =  "\Sum_{i=0}^k"
% format gimk    =  "g_{(k - i)}

< (fs * gs) !! k = Sumi0k fi * gkmi

In most cases in which we need the |k|th order derivative of |f|, we
also need to compute the lower-order derivatives.  In the stream-based
approach, therefore, we can implement multiplication as a function which
delivers the stream of all elements of the Taylor expansion of |(f * g)|.
For example:

< fs * gs  =  map mapRed (zip (inits fs) (map reverse (inits gs)))
<   where mapRed (as, bs)  =  foldr mR (zip as bs)
<         mR (a, b) res    =  a * b + res

This corresponds clearly to the formula, but is very inefficient, owing
to the presence of |reverse|.  We can derive a much more efficient
version on the basis of the following observations:

Let |gs = (g0 : gs'), gss = map reverse (inits gs)|

< map cons (zip (map head xss) (map tail xss)) = xss
< map head (tail gss)  =  gs'
< map tail (tail gss)  =  gss

By simple equational reasoning we find

< gss  =  [g0] : tail gss
<      =  [g0] : map cons (zip (map head (tail gss))
<                              (map tail (tail gss)))
<      =  [g0] : map cons (zip gs' gss)

a more efficient version of the computation.  We can also go on and
derive a better product function, but it's worth pausing here a moment
and considering the following question: how can the properties we have
used in this derivation be proven?

Normally, we associate inductively defined datatypes with proofs by
induction.  In this case, the inductive principle we read off the |data|
declaration is:

< (forall xs, a  P xs -> P (a : xs)) -> forall xs (P xs)

Unfortunately, this is incorrect: it would allow us, for instance, to
prove that there are no constant streams!

In fact, inductive principles ``work'' only for the \emph{smallest fixed
point} which satisfies the |data| declaration, which in the case of
|Stream| is \ldots the empty set.  So, in a sense, the induction
principle is correct, it just doesn't apply to the model we have of
streams.

The question of the meaning of the |Stream| |data| declaration and the
proof methods appropriate to such declarations is going to be discussed
in later lectures.

Returning to the implementation of the product of two Taylor expansions,
there is a more elegant way to proceed:

< eval x0 x (fs * gs) = eval x0 x fs * eval x0 x gs
<                     = (f0 + (x - x0) * eval x0 x fs') *
<                       (g0 + (x - x0) * eval x0 x gs')
<                     = f0 * g0 + (x - x0) * (f0 * eval x0 x gs' +
<                                            eval x0 x (fs' * gs))
<
<                     = eval x0 x ((f0*g0) :
<                       (f0 * gs' + fs' * gs)

Therefore, we can define

< fs * gs  =  (f0 * g0) : (f0 : zeros * gs' + fs' * gs)

an efficient, index-free version of the multiplication of series.

Thus, the instance declarations for arithmetic functions can be given as
follows.

> type HD a = [a]

> con x = x : repeat 0
> var x = x : con 1

> instance Num a => Num (HD a) where
>   fromInteger n = con (fromInteger n)
>   negate (f0 : fs') = -f0 : negate fs'
>   (f0 : fs') + (g0 : gs') = f0 + g0 : fs' + gs'
>   (f0 : fs') * gs @ (g0 : gs') = f0 * g0 : fs'*gs + con f0 * gs'
>   abs = undefined
>   signum = undefined

> instance Fractional a => Fractional (HD a) where
>   (f0 : fs') / (g0 : gs') = qs where qs = f0 / g0 : con (1/g0)*(fs'-qs*gs')
>   fromRational = con . fromRational

Perhaps the most useful aspect of the stream representation of series is
that it allows us to define elementary functions by means of the
differential equations they satisfy.  First, we observe that

> countFrom n = n : countFrom (n+1)
> diff (f0:fs') = zipWith (*) fs' (countFrom 1)
> integral fs = 0 : zipWith (/) fs  (countFrom 1)

Using |diff| and |integral|, we have:

> instance (Fractional a, Power a) => Power (HD a) where
>   pow gs 0 = con 1
>   pow gs (n + 1) = con (pow (head gs) (n + 1)) +
>           integral ((fromInteger (n + 1)) * pow gs n * diff gs)

> instance (Power a, Floating a) => Floating (HD a) where
>   pi                    =  con pi
>   exp gs @ (g0 : gs')   =  con (exp g0) +
>                            integral (diff gs * exp gs)
>   log                   =  undefined -- TODO: exercise
>   sqrt                  =  undefined -- TODO: exercise
>   (**)                  =  undefined -- TODO: exercise
>   logBase               =  undefined -- TODO: exercise
>   sin gs @ (g0 : gs')   =  con (sin g0) +
>                            integral (diff gs * cos gs)
>   cos gs @ (g0 : gs')   =  con (cos g0) +
>                            integral (-diff gs * sin gs)
>   tan                   =  undefined -- TODO: exercise
>   asin                  =  undefined -- TODO: exercise
>   acos                  =  undefined -- TODO: exercise
>   atan                  =  undefined -- TODO: exercise
>   sinh                  =  undefined -- TODO: exercise
>   cosh                  =  undefined -- TODO: exercise
>   tanh                  =  undefined -- TODO: exercise
>   asinh                 =  undefined -- TODO: exercise
>   acosh                 =  undefined -- TODO: exercise
>   atanh                 =  undefined -- TODO: exercise

> newtonHD :: (Ord a, Floating a) => (HD a -> HD a) -> a -> a -> a
> newtonHD f tol x
>   | abs (x - x') < tol  =  x'
>   | otherwise           =  newtonHD f tol x'
>   where fs         =  f (var x)
>         (fx, fx')  =  (fs!!0, fs!!1)
>         x'         =  x - (fx / fx')

> test2 :: R
> test2 = newtonHD foo (1e-10) 0
