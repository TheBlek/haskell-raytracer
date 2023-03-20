\begin{code}

module Vec3 where

\end{code}



\begin{code}
import Control.DeepSeq

data Vec3 = Vc3 {x, y, z:: Double} 
    deriving (Read)

instance NFData Vec3 where
    rnf (Vc3 x y z) = seq x $ seq y $ seq z ()

instance Show Vec3 where
    show (Vc3 x y z) = "(" ++ (show x) ++ " " ++ (show y) ++ " " ++ (show z) ++ ")" 

\end{code}

Basic operations with Vec3:

(я короче хз как назвать произведение ветора на скаляр ну типа правильно если чо и ещё как сделать чтобы можно было умножать скаляр то есть kprod scalar (Vc3 x y z), хотя мб это не нужно)
- Можно умножение и деление на скаляр сделать как символы, чтобы было легче. По поводу этих символов не уверен.

\begin{code}

instance Num Vec3 where
  Vc3 x1 y1 z1 + Vc3 x2 y2 z2 = Vc3 (x1+x2) (y1+y2) (z1+z2)
  Vc3 x1 y1 z1 - Vc3 x2 y2 z2 = Vc3 (x1-x2) (y1-y2) (z1-z2)
  Vc3 x1 y1 z1 * Vc3 x2 y2 z2 = Vc3 (x1*x2) (y1*y2) (z1*z2)

(*>>) :: Double -> Vec3 -> Vec3
scalar *>> (Vc3 x y z) = Vc3 (scalar * x) (scalar * y) (scalar * z)

(<<*) :: Vec3 -> Double -> Vec3
(<<*) = flip (*>>)

vc <<\ 0 = vc -- Наверное стоит валится тут?
vc <<\ scalar = vc <<* (1 / scalar)

dot :: Vec3 -> Vec3 -> Double
dot (Vc3 x1 y1 z1) (Vc3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

length_sqr :: Vec3 -> Double
length_sqr vc = dot vc vc

length :: Vec3 -> Double
length vc = sqrt $ length_sqr vc

cross :: Vec3 -> Vec3 -> Vec3
cross (Vc3 x1 y1 z1) (Vc3 x2 y2 z2) = Vc3 (y1 * z2 + z1 * y2) (x1 * z2 + z1 * x2) (x1 * y2 + y1 * x2)
 
norm :: Vec3 -> Vec3
norm vc = vc <<\ (Vec3.length vc)

near_zero :: Vec3 -> Bool
near_zero (Vc3 x y z) = (x <= eps) && (y <= eps) && (z <= eps)
    where eps = 1e-8

instance Eq Vec3 where
    a == b = length_sqr (a - b) < 1^^(-10) -- Или стоит сделать просто покомпонентное сравнение?
\end{code}

Сделал константы, как их обычно делают, но я в них вечно путаюсь. (y это вверх?)
Так что можно сделать нормально, если тебе тоже непривычно.

\begin{code}

one = Vc3 1 1 1
zero = Vc3 0 0 0

forward = Vc3 1 0 0
backward = Vc3 (-1) 0 0

right = Vc3 0 0 1
left = Vc3 0 0 (-1)

up = Vc3 0 1 0
down = Vc3 0 (-1) 0

\end{code}
