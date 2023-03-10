\begin{code}
module Sphere where

import Point
import Color
\end{code}

!!! Sphere can have "Rugged" or "Metal" material
for example:
let sphere = Sph zero zero red Rugged

\begin{code}
data Material = Metal | Rugged  
\end{code}

\begin{code}
data Sphere = Sph { center::Point, radius::Double, color::Color, material::Material } 
\end{code}
