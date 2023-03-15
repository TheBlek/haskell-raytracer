\begin{code}

module Material where

import System.Random
import MyRandom
import Control.Monad.State
import Vec3
import Ray
import Color
import Point

\end{code}

\begin{code}
data Material = Metal Color | Rugged Color  

\end{code}

\begin{code}

scatter :: (Point, Vec3, Material) -> Ray -> State StdGen (Color, Ray)
scatter (point, norm, Rugged color) ray = do
    offset <- random_vec_in_sphereS
    let out_dir = norm + offset
    if near_zero out_dir then
        return (color, Ry point norm)
    else
        return (color, Ry point out_dir)




\end{code}
