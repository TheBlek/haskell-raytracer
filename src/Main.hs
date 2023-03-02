module Main where

import System.IO

import Vec3
import Ray
import Color
import Point
import Sphere
import Hittable
import MyRandom


aspect_ratio = 16 / 9
image_height = 720
image_width = aspect_ratio * image_height

viewport_height = 2
viewport_width = aspect_ratio * viewport_height

focal_length = 1

color_ray :: [Sphere] -> Ray -> Color
color_ray objs ray = maybe background map_normal normal
    where 
        map_normal normal = make_shadow (Vc3 1 (0.3) (-1)) normal (Cl ((255 * 0.5) *>> (normal + one)))
        normal = hit_normal ray (0.001, 100) objs
        background = blend blue white ((/ viewport_height) . (+ viewport_height/2) . y . dir $ ray)

write_file :: String -> [Color] -> IO ()
write_file filename colors = withFile filename WriteMode (\handle -> do
    hPutStrLn handle "P3"
    hPutStrLn handle $ (show . floor $ image_width) ++ " " ++ (show . floor $ image_height)
    hPutStrLn handle "255"
    mapM_ (hPrint handle) colors
                                                         )

main :: IO ()
main = do
    let samples_per_pixel = 100
    let sphere = Sph (Vc3 0 0.2 (-1)) (0.5)
    let sphere2 = Sph (Vc3 0.2 0.2 (-0.6)) (0.2)
    let viewport_left_corner = Vc3 (-viewport_width/2) (-viewport_height/2) (-focal_length)
    let rays = map (\(Cl vec) -> (Cl (vec <<\ samples_per_pixel))) [multi_color u v 100|
            v <-  reverse [0, 1/(image_height - 1)..1],
            u <-  [0, 1/(image_width - 1)..1]]
                where multi_color u v 0 = color_ray [sphere, sphere2] (Ry zero zero)
                      multi_color u v n = color_ray [sphere, sphere2] (Ry zero ((viewport_left_corner + one <<* ((randomDbl n) / 500))  + (u * viewport_width) *>> (forward + one <<* ((randomDbl n) / 500))  + (v * viewport_height) *>> (up + one <<* ((randomDbl n) / 500)))) <++>  multi_color u v (n - 1)
    write_file "output.ppm" rays
 
    return ()
    
