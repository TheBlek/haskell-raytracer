\begin{code}

module Ray where

import Vec3
import Point

\end{code}

\begin{code}

data Ray = Ry {orig::Point, dir::Vec3}

instance Show Ray where
    show ray = "origin: " ++ show (orig ray) ++ " direction: " ++ show (dir ray) 

\end{code}

Можем поменять название at ну типа как ещё конечную точку назвать

\begin{code}

atPoint :: Ray -> Double -> Point
atPoint ray t = orig ray + (dir ray <<* t) 

\end{code}