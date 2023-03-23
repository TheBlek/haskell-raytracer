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
data Material = Metal Color Double | Rugged Color  | Glass Double
\end{code}

This Schlick's approximation for reflectance

\begin{code}
reflectance:: Double -> Double -> Double
reflectance cos ratio = b + (1 - b) * ((1 - cos) ** 5)
    where b = a * a
          a = (1 - ratio) / (1 + ratio)  
\end{code}


\begin{code}
refract:: Vec3 -> Vec3 -> Double -> Vec3
refract normal dir ratio = ray_out_perp + ray_out_par
    where   
        cos_theta = min (dot ((-1) *>> dir) normal) 0
        ray_out_perp = ratio *>> (dir + (cos_theta *>> normal))
        ray_out_par = (*>> normal) $ (*(-1)) $ sqrt $ abs $ subtract (length_sqr ray_out_perp) 1 

reflect :: Vec3 -> Vec3 -> Vec3
reflect ray normal = ray - (normal <<* 2 <<* dot ray normal)

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
    let scatter_dir = out_dir + fuzz *>> offset
    if dot scatter_dir normal < 0 then
        return (black, Ry point (dir ray))
    else
        return (color, Ry point scatter_dir)

scatter (point, normal, Glass ratio) ray = do
    offset <- randomDbls
    let cos_theta = min (dot ((-1) *>> dir ray) normal) 0
    let sin_theta = sqrt $ 1 - cos_theta * cos_theta
    let front_face = normal `dot` dir ray < 0
    let new_ratio = if front_face then ratio else 1 / ratio
    let cant_refract = new_ratio * sin_theta > 1
    let out_dir = if cant_refract && reflectance cos_theta new_ratio > offset
        then reflect (norm . dir $ ray) normal 
        else refract (norm normal) (norm $ dir ray) new_ratio 
    return (white, Ry point out_dir) 
   


\end{code}
