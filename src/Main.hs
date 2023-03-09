{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Main where

import System.IO
import System.Random
import Control.Monad.State

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

color_ray :: Hittable a => Int -> a -> Ray -> State Int Color
color_ray depth objs ray 
    | depth >= 20 = return $ Cl zero
    | otherwise = do
        seed <- get
        let color (point, normal) = color_ray (depth + 1) objs (Ry point (normal + randomVec3_in_sphere seed))
        let map_hit hit = (\clr -> (<<** 0.5) $ blend clr black 0.5) <$> color hit
        put $ randomInt seed 
        maybe (return background) map_hit hit
        where 
            background = blend light_blue white ((/ viewport_height) . (+ viewport_height/2) . y . norm . dir $ ray)
            hit = hit_data ray (0.0001, 100) objs

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
    let sphere = Sph (Vc3 0 0.2 (-1.5)) 0.5
    let sphere2 = Sph (Vc3 0 (-100.5) 0) 100
    let viewport_left_corner = Vc3 (-viewport_width/2) (-viewport_height/2) (-focal_length)
    let rays = [(\(Cl vec) -> Cl (vec <<\ samples_per_pixel)) $ multi_color u v (floor samples_per_pixel)|
            v <-  reverse [0, 1/(image_height - 1)..1],
            u <-  [0, 1/(image_width - 1)..1]]
                where multi_color u v 0 = Cl zero
                      multi_color u v n = evalState (color_ray 0 [sphere, sphere2] 
                        (Ry zero (
                            viewport_left_corner 
                            + forward <<* (u * viewport_width + randomDbl (2*n::Int) / (image_width - 1))
                            + up <<* (v * viewport_height + randomDbl (2*n-1::Int) / (image_height - 1))
                                 )
                        )) n <++>  multi_color u v (n - 1)
    write_file "output.ppm" rays
 
    return ()
    
