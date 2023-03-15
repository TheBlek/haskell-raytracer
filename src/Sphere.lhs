\begin{code}
module Sphere where

import Point
import Color
import Material
\end{code}

!!! Sphere can have "Rugged" or "Metal" material
for example:
let sphere = Sph zero zero red Rugged

\begin{code}
data Sphere = Sph { center::Point, radius::Double, material::Material } 
\end{code}
