\begin{code}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Color where
import System.IO
import Vec3

\end{code}

\begin{code}

data Color = Cl {color::Vec3}

instance Show Color where
    show (Cl (Vc3 r g b)) = show (floor r) ++ " " ++ show (floor g) ++ " " ++ show (floor b)

(<++>):: Color -> Color -> Color
(<++>) (Cl vec1) (Cl vec2) = Cl (vec1 + vec2)

(<<**):: Color -> Double -> Color
(<<**) (Cl vec) num = Cl (vec <<* num)

absorb :: Color -> Color -> Color
absorb (Cl cl1) (Cl cl2) = Cl (cl1 * cl2 <<\ 255)

\end{code}

some constants

\begin{code}

blend :: Color -> Color -> Double -> Color
blend cl1 cl2 x 
    | x <= 1 && x >= 0 = Cl (x *>> (color cl1) + (1 - x) *>> (color cl2)) 
    | otherwise = undefined

make_valid :: Color -> Color
make_valid (Cl (Vc3 r g b)) = Cl (Vc3 (fromIntegral r_i) (fromIntegral g_i) (fromIntegral b_i))
    where (r_i, g_i, b_i) = (round r, round g, round b) 

black = Cl zero 

white = Cl (Vc3 255 255 255)

red = Cl (Vc3 255 0 0)

blue = Cl (Vc3 0 0 255)
light_blue = Cl (Vc3 128 179 255)

green = Cl (Vc3 0 255 0)

\end{code}
