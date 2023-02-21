\begin{code}

module Ray where

import Vec3
import Point
import Sphere

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

ray_sphere_discriminant :: Ray -> Sphere -> Double
ray_sphere_discriminant (Ry origin dir) (Sph center r) = discriminant
    where 
        origin_spherical = origin - center -- (A - C)
        discriminant = (dir `dot` origin_spherical) ^ 2 - (dir `dot` dir) * ((origin_spherical `dot` origin_spherical) - r*r) 

intersects :: Ray -> Sphere -> Bool
intersects ray = (>= 0) . (ray_sphere_discriminant ray)
        

\end{code}
