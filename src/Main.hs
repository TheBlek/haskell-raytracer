module Main where

import System.IO
import System.Random

import Vec3
import Ray
import Color
import Point
import Sphere
import Hittable
import MyRandom


aspect_ratio = 16 / 9
image_height = 360
image_width = aspect_ratio * image_height

viewport_height = 2
viewport_width = aspect_ratio * viewport_height

focal_length = 1

color_ray :: Hittable a => a -> Ray -> Color
color_ray objs ray = maybe background map_normal normal
    where 
        map_normal normal = make_shadow (Vc3 1 (0.3) (-1)) normal (Cl ((255 * 0.5) *>> (normal + one)))
        normal = hit_normal ray (0.0, 100) objs
        background = blend blue white ((/ viewport_height) . (+ viewport_height/2) . y . norm . dir $ ray)

write_file :: String -> [Color] -> IO ()
write_file filename colors = withFile filename WriteMode (\handle -> do
    hPutStrLn handle "P3"
    hPutStrLn handle $ (show . floor $ image_width) ++ " " ++ (show . floor $ image_height)
    hPutStrLn handle "255"
    mapM_ (hPrint handle) colors
                                                         )

main :: IO ()
main = do
    let samples_per_pixel = 200
    let sphere = Sph (Vc3 0 0.2 (-1.5)) (0.5)
    let sphere2 = Sph (Vc3 0.4 0.2 (-1.3)) (0.3)
    let viewport_left_corner = Vc3 (-viewport_width/2) (-viewport_height/2) (-focal_length)
    let rays = map (\(Cl vec) -> (Cl (vec <<\ samples_per_pixel))) [multi_color u v (floor samples_per_pixel)|
            v <-  reverse [0, 1/(image_height - 1)..1],
            u <-  [0, 1/(image_width - 1)..1]]
                where multi_color u v 0 = Cl zero
                      multi_color u v n = color_ray [sphere, sphere2]
                        (Ry zero (
                            viewport_left_corner 
                            + forward <<* (u * viewport_width + (randomDbl (2*n::Int)) / (image_width - 1))
                            + up <<* (v * viewport_height + (randomDbl (2*n-1::Int)) / (image_height - 1))
                                 )
                        ) <++>  multi_color u v (n - 1)
    write_file "output.ppm" rays
 
    return ()
    
