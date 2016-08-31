module Transformations where
  
import System.Random


type Transform = (Float, Float) -> (Float, Float)
type ScaledTransform = Float -> Transform

sinusoidal :: ScaledTransform
sinusoidal _ (x, y) =  (sin x, sin y)


spherical:: ScaledTransform
spherical sF (x, y) = (x', y')
  where 
    r = (radius x y) /2
    x' = normSpherical sF r * x
    y' = normSpherical sF r * y


normSpherical :: Float -> Float -> Float
normSpherical sF =  norm (-m) m (-sF) sF
   where m = (radius (-1) 1) / 2 


swirl :: ScaledTransform
swirl sF (x, y) = (normSwirl sF x', normSwirl sF y')
  where
     r = (radius x y) /2
     rSin = sin (r*r)
     rCos = cos (r*r)
     x' = x * rSin - y * rCos
     y' = x * rCos - y * rSin
  

normSwirl:: Float ->  Float -> Float
normSwirl sF = norm (-2) 2 (-sF) sF


pdj :: ScaledTransform
pdj sF (x, y) = (x'', y'')
  where 
    p1 = 1.0111
    p2 = -1.011
    p3 = 2.08
    p4 = 10.2
    x' = sin (p1 * y) - cos (p2 * x)
    y' = sin (p3 * x) - cos (p4 * y)
    x'' = normSwirl sF x'
    y'' = normSwirl sF y'

julia :: Float -> ScaledTransform
julia rdm sF (x, y) = (x', y')
  where
    r = mag (x, y)
    theta = 0.5 * (atan2 x y) + (realToFrac $ floor rdm) * pi
    x' = normJulia  sF (r * cos theta)
    y' = normJulia  sF (r * sin theta)

normJulia:: Float ->  Float -> Float
normJulia sF = norm (-pi) (pi) (-sF) sF

radius :: Float -> Float -> Float
radius x y= sqrt (x*x + y*y)


norm :: Float -> Float -> Float -> Float -> Float -> Float 
norm  start1 stop1 start2 stop2 value = start2 + (stop2 - start2) * ((value - start1) / (stop1 - start1))



calc :: Float -> Transform -> Transform -> (Float -> Float -> Float) -> (Float, Float) -> (Float, Float)
calc sF v w f (x, y) = (x', y')
  where 
    (x1, y1) = v (x, y)
    (x2, y2) = w (x, y)
    x' = norm (-2 * sF) (2 * sF) (-sF) sF (f x1  x2)
    y' = norm (-2 * sF) (2 * sF) (-sF) sF (f y1  y2)
    
sub ::Float -> Transform -> Transform ->  (Float, Float) -> (Float, Float)
sub sF v w (x, y) = calc sF v w (-) (x, y)


add ::Float -> Transform -> Transform ->  (Float, Float) -> (Float, Float)
add sF v  w (x, y) = calc sF v w (+) (x, y)


mult ::Float -> Transform -> Transform ->  (Float, Float) -> (Float, Float)
mult sF v w (x, y) = calc sF v w (*) (x, y)


divd ::Float -> Transform -> Transform ->  (Float, Float) -> (Float, Float)
divd sF v w (x, y) = calc sF v w (/) (x, y)


mag :: (Float, Float) -> Float
mag (x, y )=  sqrt (x*x + y*y)
 
