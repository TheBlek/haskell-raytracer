\begin{code}
module MyRandom where

import System.IO
import System.Random
import Vec3

randomInt :: Int -> Int
randomInt seed = fst $ random $ mkStdGen seed

randomDbl :: Int -> Double
randomDbl seed = fst $ randomR (-1, 1) $ mkStdGen seed

randomVec3 :: Int -> Vec3
randomVec3 seed = Vc3 (randomDbl seed) (randomDbl $ seed * 2 - 1) (randomDbl $ seed * 2)

randomVec3_in_sphere :: Int -> Vec3
randomVec3_in_sphere seed = head $ filter (\vec -> length_sqr vec <= 1 ) [randomVec3 new_seed | new_seed <- [(randomInt seed)..]]
\end{code}
