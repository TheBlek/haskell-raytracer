\begin{code}

module Ray where

import Vec3
import Point
import Sphere
import Data.Maybe

\end{code}

\begin{code}

data Ray = Ry {orig::Point, dir::Vec3}

instance Show Ray where
    show ray = "origin: " ++ show (orig ray) ++ " direction: " ++ show (dir ray) 

\end{code}

Можем поменять название at ну типа как ещё конечную точку назвать

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

ray_sphere_intersection :: Ray -> Sphere -> Maybe Double
ray_sphere_intersection (Ry origin dir) (Sph center r) = 
    if discriminant >= 0 then
        Just ((-b_half - discriminant) / a)
    else
        Nothing
    where 
        origin_spherical = origin - center -- (A - C)
        b_half = dir `dot` origin_spherical
        a = length_sqr dir
        c = (length_sqr origin_spherical) - r*r
        discriminant = b_half ^ 2 - a * c 


intersects :: Ray -> Sphere -> Bool
intersects ray = (maybe False (>=0)) . (ray_sphere_intersection ray)


\end{code}
