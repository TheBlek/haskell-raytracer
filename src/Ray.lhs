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

D = (b * (A - C))^2 - (b * b) * ((A - C) * (A - C) - R^2)
where 
b - direction of ray
A - origin of ray
C - center of sphere
R - radius of sphere

If discriminant is negative then there is no intersection with sphere

\begin{code}

sphere_intersection :: Ray -> Sphere -> Maybe Double
sphere_intersection (Ry origin dir) (Sph center r) = 
    if discriminant >= 0 then
        Just ((-b_half - sqrt discriminant) / a)
    else
        Nothing
    where 
        origin_spherical = origin - center -- (A - C)
        b_half = dir `dot` origin_spherical
        a = length_sqr dir
        c = (length_sqr origin_spherical) - r*r
        discriminant = b_half*b_half - a * c 


intersects :: Ray -> Sphere -> Bool
intersects ray = maybe False (>=0) . sphere_intersection ray

sphere_intersection_point :: Ray -> Sphere -> Maybe Point
sphere_intersection_point ray =  sphere_intersection ray >=> Just . atPoint ray

sphere_intersection_normal :: Ray -> Sphere -> Maybe Vec3
sphere_intersection_normal ray sphere = sphere_intersection_point ray sphere
    >>= Just . (subtract $ center sphere) 
    >>= Just . norm 
\end{code}


\begin{code}

make_shadow :: Vec3 -> Vec3 -> Color -> Color
make_shadow light normal (Cl vec) = Cl (vec <<* (abs(norm light `dot` norm normal) ** 3))


\end{code}
