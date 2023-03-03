\begin{code}
module MyRandom where

import System.IO
import System.Random


randomDbl :: Int -> Double
randomDbl seed = fst $ randomR (-1, 1) $ mkStdGen seed
\end{code}
