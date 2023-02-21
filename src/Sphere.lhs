\begin{code}
module Sphere where

import Point
\end{code}

\begin{code}
data Sphere = Sph { center::Point, radius::Double }
    deriving (Show)
\end{code}
