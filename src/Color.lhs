\begin{code}

module Color where

import Vec3

\end{code}

\begin{code}

data Color = Cl {color::Vec3}

instance Show Color where
    show (Cl (Vc3 r g b)) = show (floor r) ++ " " ++ show (floor g) ++ " " ++ show (floor b)

\end{code}

some constants

\begin{code}

blend :: Color -> Color -> Double -> Color
blend cl1 cl2 x = Cl (x *>> (color cl1) + (1 - x) *>> (color cl2)) 

make_valid :: Color -> Color
make_valid (Cl (Vc3 r g b)) = Cl (Vc3 (fromIntegral r_i) (fromIntegral g_i) (fromIntegral b_i))
    where (r_i, g_i, b_i) = (round r, round g, round b) 

black = Cl zero 

white = Cl (Vc3 255 255 255)

red = Cl (Vc3 255 0 0)

blue = Cl (Vc3 0 0 255)

green = Cl (Vc3 0 255 0)

\end{code}
