module Main where

import System.IO

import Vec3
import Ray
import Color
import Point

aspect_ratio = 16 / 9
image_height = 360
image_width = aspect_ratio * image_height

viewport_height = 2
viewport_width = aspect_ratio * viewport_height

focal_length = 1

color_ray :: Ray -> Color
color_ray ray = blend white blue ((/ viewport_height) . (+ viewport_height/2) . y . norm . dir $ ray)

write_file :: String -> [Color] -> IO ()
write_file filename colors = withFile filename WriteMode (\handle -> do
    hPutStrLn handle "P3"
    hPutStrLn handle $ (show . floor $ image_width) ++ " " ++ (show . floor $ image_height)
    hPutStrLn handle "255"
    mapM_ (hPrint handle) (map make_valid colors)
                                                         )
    

main :: IO ()
main = do
    let viewport_left_corner = Vc3 (-viewport_width/2) (-viewport_height/2) (-focal_length)
    let rays = [Ry zero (viewport_left_corner + u *>> forward + v *>> up) |
            v <- [0, 1/(image_height - 1)..1],
            u <- [0, 1/(image_width - 1)..1]]
    let colors = map color_ray rays
    
    write_file "output.ppm" colors

    return ()
