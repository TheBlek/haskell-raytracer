\begin{code}
module MyRandom where

import System.IO
import System.Random


randomDbl :: Int -> Double
randomDbl seed = (subtract 1) $ head $ randoms $ mkStdGen seed
\end{code}