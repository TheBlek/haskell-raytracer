\begin{code}

module Ray where

import Vec3
import Point
import Sphere
import Control.Monad
import Color

\end{code}

\begin{code}

data Ray = Ry {orig::Point, dir::Vec3}

instance Show Ray where
    show ray = "origin: " ++ show (orig ray) ++ " direction: " ++ show (dir ray) 

\end{code}



\begin{code}

atPoint :: Ray -> Double -> Point
atPoint ray t = orig ray + (dir ray <<* t) 

\end{code}


\begin{code}

make_shadow :: Vec3 -> Vec3 -> Color -> Color
make_shadow light normal (Cl vec) = Cl (vec <<* (abs(norm light `dot` norm normal) ** 3))


\end{code}
