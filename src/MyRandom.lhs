\begin{code}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module MyRandom where

import System.IO
import System.Random
import Vec3
import Control.Monad.State

randomInt :: Int -> Int
randomInt seed = fst $ random $ mkStdGen seed

randomDbl :: Int -> Double
randomDbl seed = fst $ randomR (-1, 1) $ mkStdGen seed

randomVec3 :: Int -> Vec3
randomVec3 seed = Vc3 (randomDbl seed) (randomDbl $ seed * 2 - 1) (randomDbl $ seed * 2)

randomVec3_in_sphere :: Int -> Vec3
randomVec3_in_sphere seed = head $ filter (\vec -> length_sqr vec <= 1 ) [randomVec3 new_seed | new_seed <- [(randomInt seed)..]]

randomS :: (Random a) => State StdGen a
randomS = do
    (x, g) <- gets random
    put g
    return x

randomRS :: (Random a) => (a, a) -> State StdGen a
randomRS range = do
    (x, g) <- gets (randomR range)
    put g
    return x

random_vec :: State StdGen Vec3
random_vec = Vc3 <$> randomRS (-1, 1) <*> randomRS (-1, 1) <*> randomRS (-1, 1)

random_vec_in_sphereS :: State StdGen Vec3
random_vec_in_sphereS = do
    vec <- random_vec
    if length_sqr vec <= 1 then return vec
    else random_vec_in_sphereS

randomDbls :: State StdGen Double
randomDbls = do
    rand <- randomRS (0, 1)
    return rand
--head . filter (\vec -> length_sqr vec <= 1) <$> sequence [random_vec | i <- [1..]]
\end{code}
