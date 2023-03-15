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
data Material = Metal Color Double | Rugged Color  

\end{code}

\begin{code}

reflect :: Vec3 -> Vec3 -> Vec3
reflect ray normal = ray - normal <<* 2 <<* dot ray normal

scatter :: (Point, Vec3, Material) -> Ray -> State StdGen (Color, Ray)
scatter (point, norm, Rugged color) ray = do
    offset <- random_vec_in_sphereS
    let out_dir = norm + offset
    if near_zero out_dir then
        return (color, Ry point norm)
    else
        return (color, Ry point out_dir)

scatter (point, normal, Metal color fuzz) ray = do
    let out_dir = reflect (norm . dir $ ray) normal
    offset <- random_vec_in_sphereS
    return (color, Ry point (out_dir + fuzz *>> offset))





\end{code}
