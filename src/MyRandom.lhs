\begin{code}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module MyRandom where

import System.IO
import System.Random
import Vec3
import Control.Monad.State

randomS :: (Random a) => State StdGen a
randomS = do
    (x, g) <- gets random
    put g
    return x

{-# INLINE randomRS #-}
randomRS :: (Random a) => (a, a) -> State StdGen a
randomRS range = do
    (x, g) <- gets (randomR range)
    put g
    return x

{-# INLINE random_vec #-}
random_vec :: State StdGen Vec3
random_vec = Vc3 <$> randomRS (-1, 1) <*> randomRS (-1, 1) <*> randomRS (-1, 1)

random_vec_in_sphereS :: State StdGen Vec3
random_vec_in_sphereS = do
    vec <- random_vec
    if length_sqr vec <= 1 then return vec
    else random_vec_in_sphereS

randomDbls :: State StdGen Double
randomDbls = randomRS (0, 1) 
--head . filter (\vec -> length_sqr vec <= 1) <$> sequence [random_vec | i <- [1..]]
\end{code}
