module Fix where

import Data.Function

-- f :: (a -> b -> a) -> (c -> d -> c -> e)
-- f x = x x

fib' _ 0 = 1
fib' _ 1 = 1
fib' f n = (f (n-1)) + (f (n-2))

myMap g [] = []
myMap g (x:xs) = (g x):(myMap g xs)

myMap' f g [] = []
myMap' f g (x:xs) = (g x):(f g xs)

myFold e _ [] = e
myFold e g (x:xs) = myFold (e `g` x) g xs

myFold' f e _ [] = e
myFold' f e g (x:xs) = f (g e x) g xs

myFoldr e _ [] = e
myFoldr e g (x:xs) = x `g` (myFoldr e g xs)

flup f 0 = []
flup f n = n:(f (n-1))

ones = 1:ones
twos = 2:twos
