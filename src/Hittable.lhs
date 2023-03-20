\begin{code}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
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
import Data.Functor
import Color
import Material

\end{code}

Класс объектов, с которыми может пересекаться луч

\begin{code}

type HitData = (Point, Vec3, Material)

class Hittable a where
    hit_normal :: Ray -> (Double, Double) -> a -> Maybe Vec3

    hit_dist :: Ray -> (Double, Double) -> a -> Maybe Double

    hit_point :: Ray -> (Double, Double) -> a -> Maybe Point

    hit_material :: Ray -> (Double, Double) -> a -> Maybe Material

    hit_point ray bounds = return . atPoint ray <=< hit_dist ray bounds

    hit_data :: Ray -> (Double, Double) -> a -> Maybe HitData
    hit_data ray bounds obj =  case hit_point ray bounds obj of
        Nothing -> Nothing
        (Just point) -> Just (point, fromJust $ hit_normal ray bounds obj, fromJust $ hit_material ray bounds obj)

    hits :: Ray -> (Double, Double) -> a -> Bool
    hits r bounds = isJust . hit_point r bounds

\end{code}

Просто достал функции из Ray.lhs и переименовал

\begin{code}

instance Hittable Sphere where
    hit_dist ray (tmin, tmax) sph = sphere_intersection ray sph >>=
        (\(x0, x1) ->  if x0 < tmin || x0 > tmax then
                            if x1 > tmax || x1 < tmin then
                                Nothing
                            else
                                Just x1
                       else
                            Just x0
        )

    hit_normal ray bounds sphere = hit_point ray bounds sphere
        <&> norm . subtract (center sphere)
        <&> (\normal -> normal <<* (negate . signum . dot (dir ray) $ normal))
    
    hit_material ray bounds sphere = hit_dist ray bounds sphere >> return (material sphere)

    hit_data ray bounds sphere = hit_dist ray bounds sphere
        <&> (\dist -> (point dist, normal (outward_normal dist), material sphere))
        where point = atPoint ray
              outward_normal dist = norm $ point dist - center sphere
              normal outw = (negate . signum . dot (dir ray) $ outw) *>> outw
            
\end{code}


\begin{code}

hit_nearest_sph
  :: (Foldable t, Hittable a) =>
     Ray.Ray -> (Double, Double) -> t a -> (Double, Maybe a)

hit_nearest_sph ray (tmin, tmax) = foldl(\prev@(nearest, _) el ->
    fromMaybe prev
        (hit_dist ray (tmin, nearest) el <&> (,Just el))
        ) (tmax, Nothing)

instance (Hittable a) => Hittable [a] where
    hit_dist ray (tmin, tmax) = (\x -> if x == tmax then Nothing else return x)
        . foldl(\nearest el ->
            fromMaybe nearest (hit_dist ray (tmin, nearest) el)
        ) tmax

    hit_normal ray (tmin, tmax) obj = hit_normal ray (tmin, tmax) <=< snd 
        $ hit_nearest_sph ray (tmin, tmax) obj

    hit_material ray (tmin, tmax) obj = hit_material ray (tmin, tmax) <=< snd
        $ hit_nearest_sph ray (tmin, tmax) obj

\end{code}

D = (b * (A - C))^2 - (b * b) * ((A - C) * (A - C) - R^2)
where 
b - direction of ray
A - origin of ray
C - center of sphere
R - radius of sphere

If discriminant is negative then there is no intersection with sphere

\begin{code}

sphere_intersection :: Ray -> Sphere -> Maybe (Double, Double)
sphere_intersection (Ry origin dir) (Sph center r _ ) = 
    if discriminant >= 0 then
        Just (
            (-b_half - sqrt discriminant) / a,
            (-b_half + sqrt discriminant) / a
        ) 
    else
        Nothing
    where 
        origin_spherical = origin - center -- (A - C)
        b_half = dir `dot` origin_spherical
        a = length_sqr dir
        c = length_sqr origin_spherical - r*r
        discriminant = b_half*b_half - a * c 

\end{code}
