module Main where

import ImprovedNoise

import Codec.Picture
import Codec.Picture.Types
import System.Environment

-- | Convert an image to xkcd style by adding perlin noise to
--   each pixels coordinates.
xkcd :: Image PixelRGBA8 -> Image PixelRGBA8
xkcd img = generateImage getPixel w h
  where
    (w, h)        = (imageWidth img, imageHeight img)
    bound upper n = max 0 (min n (upper - 1))
    getPixel x y  = pixelAt img (bound w (x + deform 0    x y)) 
                                (bound h (y + deform 7919 x y))
    deform s a b  = round $ 4 * pnoise (realToFrac (a+s) / 521) 
                                       (realToFrac (b+s) / 521) 0 

main :: IO ()
main = do
  dImg <- readImage . head =<< getArgs
  writePng "output.png" $ case dImg of
    Left msg -> error msg
    Right (ImageRGB8  img) -> xkcd (promoteImage img)
    Right (ImageRGBA8 img) -> xkcd img
    Right _                -> error "png images only please."
