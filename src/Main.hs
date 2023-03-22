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
import Material

import MyRandom


aspect_ratio = 16 / 9
image_height = 180
image_width = aspect_ratio * image_height

viewport_height = 2
viewport_width = aspect_ratio * viewport_height

viewport_left_corner = Vc3 (-viewport_width/2) (-viewport_height/2) (-focal_length)

focal_length = 1

color_ray :: Hittable a => Int -> a -> Ray -> State StdGen Color
color_ray depth objs ray 
    | depth >= 50 = return $ Cl zero
    | otherwise = do
        let follow_ray = color_ray (depth+1) objs 
        let map_hit hit = scatter hit ray >>= (\(cl, new_ray) -> absorb cl <$> follow_ray new_ray) 
        let hit = hit_data ray (0.001, 100) objs
        let background = blend light_blue white ((/ viewport_height) . (+ viewport_height/2) . y . norm . dir $ ray)

        maybe (return background) map_hit hit

gen_ray :: Double -> Double -> State StdGen Ray
gen_ray u v = do
    u_offset <- randomRS (0, 1)
    v_offset <- randomRS (0, 1)
    return $ Ry zero (
        viewport_left_corner 
        + forward <<* (u + u_offset / (image_width - 1)) <<* viewport_width
        + up <<* (v + v_offset / (image_height - 1)) <<* viewport_height
                     )

multi_color :: (Hittable a) => a -> Double -> Double -> Int -> State StdGen Color
multi_color _ _ _ 0 = return black
multi_color objs u v n = (<++>) 
    <$> multi_color objs u v (n - 1) 
    <*> (gen_ray u v >>= color_ray 0 objs)

write_file :: String -> [Color] -> IO ()
write_file filename colors = withFile filename WriteMode (\handle -> do
    hPutStrLn handle "P3"
    hPutStrLn handle $ (show . floor $ image_width) ++ " " ++ (show . floor $ image_height)
    hPutStrLn handle "255"
    mapM_ (hPrint handle) colors
                                                         )

main :: IO ()
main = do
    let samples_per_pixel = 50
    let material1 = Glass 1.5
    let material2 = Rugged green
    let material3 = Metal light_blue 0.2
    let material4 = Metal (Cl (Vc3 204 153 51)) 1
    let sphere = Sph (Vc3 0 0 (-1)) 0.5 material1
    let sphere2 = Sph (Vc3 0 (-100.5) 0) 100 material2
    let sphere3 = Sph (Vc3 (-1) 0 (-1)) 0.5 material3
    let sphere4 = Sph (Vc3 1 0 (-1)) 0.5 material4
    let objs = [sphere, sphere2, sphere3, sphere4]
    let accumulated_color = [multi_color objs u v (floor samples_per_pixel)|
            v <-  reverse [0, 1/(image_height - 1)..1], 
            u <-  [0, 1/(image_width - 1)..1]]
    let colors = mapM (fmap (adjust_gamma 2 . average samples_per_pixel)) accumulated_color
    write_file "output.ppm" $ evalState colors (mkStdGen 0)
 
    return ()
    
