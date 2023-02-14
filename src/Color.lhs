\begin{code}

module Color where

import Vec3

\end{code}

\begin{code}

data Color = Cl {color::Vec3}

instance Show Color where
    show clr = show (color clr)

\end{code}

some constants

\begin{code}

black = Cl zero 

white = Cl (Vc3 255 255 255)

red = Cl (Vc3 255 0 0)

blue = Cl (Vc3 0 0 255)

green = Cl (Vc3 0 255 0)

\end{code}