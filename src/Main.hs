module Main where

import System.IO

import Vec3
import Ray
import Color
import Point
import Sphere
import Hittable

aspect_ratio = 16 / 9
image_height = 360
image_width = aspect_ratio * image_height

viewport_height = 2
viewport_width = aspect_ratio * viewport_height

focal_length = 1

color_ray :: Sphere -> Ray -> Color
color_ray sphere ray = maybe background map_normal point
    where 
        map_normal point = make_shadow (Vc3 1 (0.3) (-1)) (center sphere - point) (Cl ((255 * 0.5) *>> (point + one)))
        point = hit_normal ray (0.001, 100) sphere
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
    let sphere = Sph (Vc3 0 0.2 (-1)) (0.5)
    let viewport_left_corner = Vc3 (-viewport_width/2) (-viewport_height/2) (-focal_length)
    let rays = [Ry zero (viewport_left_corner + (u * viewport_width) *>> forward + (v * viewport_height) *>> up) |
            v <- reverse [0, 1/(image_height - 1)..1],
            u <- [0, 1/(image_width - 1)..1]]
    let colors = map (color_ray sphere) rays

    write_file "output.ppm" colors

    return ()
    
    --hello
