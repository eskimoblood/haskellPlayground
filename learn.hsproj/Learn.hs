import Graphics.Rasterific.Texture
import Graphics.Rasterific hiding (transform)

import Codec.Picture

import System.Random

import Transformations




width = 1000
height = 1000
size = 1000
sF = 3


color = (0, 0, 0, 25)

cartProd :: Float ->  Float -> [(Float, Float)]
cartProd size d = [ ((norm  0 size (-sF) sF x), (norm  0 size (-sF) sF y)) | x <- [0..(size) ], y <- [0..(size)]]


transform :: [(Float, Float)] -> [(Float, Float)]
transform c= map t (zip r c)
  where
    r = randomRs (0, 2) (mkStdGen 3) 

t :: (Float , (Float, Float)) -> (Float, Float)
t (r, p) = sub sF (pdj sF) (julia r sF) p

scale  :: [(Float, Float)] -> [(Float, Float)]
scale points = map normalizePoint points 


normalizePoint :: (Float, Float) -> (Float, Float)
normalizePoint (x, y) = (n x, n y) 
  where 
   n = norm (-sF) sF 50 (width-50)


toPath :: [(Float, Float)] -> [((Int, Int, Int, Int), Point)]
toPath points = map (\(r, (x,y)) -> (color,  V2 (x+1 + r) y)) (zip r points)
  where
    r = randomRs (-2.1, 2.1) (mkStdGen 3)  


pointCoords = toPath $ scale $ transform $ cartProd size 3


drawPicture :: Float -> [((Int, Int, Int, Int), Point)] -> Image PixelRGBA8
drawPicture linewidth picture
  = renderDrawing  width height (toColour (217, 214, 209, 255)) $
      mapM_ (\(col, path) -> withTexture (uniformTexture $ toColour col) (drawPath path)) picture
  where
    drawPath points    = fill $ circle  points 0.5
    toColour (a,b,c,d) = PixelRGBA8 (fromIntegral a) (fromIntegral b) (fromIntegral c) (fromIntegral d)
