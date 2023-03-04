\begin{code}
{-# LANGUAGE TupleSections #-}
module Hittable where 

import Ray
import Sphere
import Point
import Vec3
import Control.Monad
import Data.Maybe
import Safe
import Data.List
\end{code}

Класс объектов, с которыми может пересекаться луч

\begin{code}
class Hittable a where
    hit_normal :: Ray -> (Double, Double) -> a -> Maybe Vec3

    hit_dist :: Ray -> (Double, Double) -> a -> Maybe Double

    hit_point :: Ray -> (Double, Double) -> a -> Maybe Point
    hit_point ray bounds = return . atPoint ray <=< hit_dist ray bounds

    hits :: Ray -> (Double, Double) -> a -> Bool
    hits r obj = isJust . (hit_point r obj)

\end{code}

Просто достал функции из Ray.lhs и переименовал

\begin{code}
instance Hittable Sphere where
    hit_dist ray (tmin, tmax) = headMay 
        . filter (>=tmin) . filter (<=tmax)
        <=< sphere_intersection ray

    hit_normal ray bounds sphere = hit_point ray bounds sphere
        >>= return . norm . (subtract $ center sphere) 
        >>= return . (\normal -> normal <<* (negate . signum . dot (dir ray) $ normal))

\end{code}

\begin{code}

instance (Hittable a) => Hittable [a] where
    hit_dist ray (tmin, tmax) = (\x -> if x == tmax then Nothing else return x)
        . foldl(\nearest el ->
            maybe nearest id (hit_dist ray (tmin, nearest) el)
        ) tmax


    hit_normal ray (tmin, tmax) = hit_normal ray (tmin, tmax) <=< snd
        . foldl(\prev@(nearest, _) el ->
            maybe prev id 
                    (hit_dist ray (tmin, nearest) el >>= return . (,Just el))
        ) (tmax, Nothing)
\end{code}

D = (b * (A - C))^2 - (b * b) * ((A - C) * (A - C) - R^2)
where 
b - direction of ray
A - origin of ray
C - center of sphere
R - radius of sphere

If discriminant is negative then there is no intersection with sphere

\begin{code}

sphere_intersection :: Ray -> Sphere -> Maybe [Double]
sphere_intersection (Ry origin dir) (Sph center r) = 
    if discriminant >= 0 then
        Just [
            (-b_half - sqrt discriminant) / a,
            (-b_half + sqrt discriminant) / a
        ]
    else
        Nothing
    where 
        origin_spherical = origin - center -- (A - C)
        b_half = dir `dot` origin_spherical
        a = length_sqr dir
        c = (length_sqr origin_spherical) - r*r
        discriminant = b_half*b_half - a * c 
\end{code}
