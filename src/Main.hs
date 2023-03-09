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
image_height = 360
image_width = aspect_ratio * image_height

viewport_height = 2
viewport_width = aspect_ratio * viewport_height

viewport_left_corner = Vc3 (-viewport_width/2) (-viewport_height/2) (-focal_length)

focal_length = 1

color_ray :: Hittable a => Int -> a -> Ray -> State StdGen Color
color_ray depth objs ray 
    | depth >= 10 = return $ Cl zero
    | otherwise = do
        offset <- random_vec_in_sphereS

        let color (point, normal) = color_ray (depth + 1) objs (Ry point (normal + offset))
        let map_hit hit = (\clr -> blend clr black 0.5) <$> color hit
        let background = blend light_blue white ((/ viewport_height) . (+ viewport_height/2) . y . norm . dir $ ray)
        let hit = hit_data ray (0.0001, 100) objs

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
    let samples_per_pixel = 100
    let sphere = Sph (Vc3 0 0.2 (-1.5)) 0.5
    let sphere2 = Sph (Vc3 0 (-100.5) 0) 100
    let objs = [sphere, sphere2]
    let accumulated_color = [multi_color objs u v (floor samples_per_pixel)|
            v <-  reverse [0, 1/(image_height - 1)..1], 
            u <-  [0, 1/(image_width - 1)..1]]
    let colors = mapM (fmap (\(Cl x) -> Cl (x <<\ samples_per_pixel))) accumulated_color
    write_file "output.ppm" $ evalState colors (mkStdGen 0)
 
    return ()
    
