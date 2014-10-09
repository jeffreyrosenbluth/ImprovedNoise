-- | Based on Java reference implementation - (c) 2002 Ken Perling

module ImprovedNoise 
  (improvedNoise, improvedNoiseI) where

import qualified Data.Vector.Storable as V
import           Data.Vector.Storable    (Vector, fromList, (!))
import           Data.Bits               ((.&.), shiftR, shiftL)

permutation :: Vector Int
permutation = fromList [ 151,160,137,91,90,15,
   131,13,201,95,96,53,194,233,7,225,140,36,103,30,69,142,8,99,37,240,21,10,23,
   190, 6,148,247,120,234,75,0,26,197,62,94,252,219,203,117,35,11,32,57,177,33,
   88,237,149,56,87,174,20,125,136,171,168, 68,175,74,165,71,134,139,48,27,166,
   77,146,158,231,83,111,229,122,60,211,133,230,220,105,92,41,55,46,245,40,244,
   102,143,54, 65,25,63,161, 1,216,80,73,209,76,132,187,208, 89,18,169,200,196,
   135,130,116,188,159,86,164,100,109,198,173,186, 3,64,52,217,226,250,124,123,
   5,202,38,147,118,126,255,82,85,212,207,206,59,227,47,16,58,17,182,189,28,42,
   223,183,170,213,119,248,152, 2,44,154,163, 70,221,153,101,155,167, 43,172,9,
   129,22,39,253, 19,98,108,110,79,113,224,232,178,185, 112,104,218,246,97,228,
   251,34,242,193,238,210,144,12,191,179,162,241, 81,51,145,235,249,14,239,107,
   49,192,214, 31,181,199,106,157,184, 84,204,176,115,121,50,45,127, 4,150,254,
   138,236,205,93,222,114,67,29,24,72,243,141,128,195,78,66,215,61,156,180 ]

p :: Vector Int
p = permutation V.++ permutation

fade :: Double -> Double
fade t = t * t * t * (t * (t * 6 -15) + 10)

lerp :: Double -> Double -> Double -> Double
lerp t a b = a + t * (b - a)

grad :: Num a => Int -> a -> a -> a -> a
grad hash x y z
  | h .&. 1 == 0, h .&. 2 == 0 = u + v
  | h .&. 1 == 0             = u - v
  |             h .&. 2 == 0 = v - u
  | otherwise                = - (u + v)
  where
    -- Convert lo 4 bits of hase code into 12 gradient directions.
    h = hash .&. 15
    u | h < 8     = x 
      | otherwise = y
    v | h < 4             = y
      | h ==12 || h == 14 = x
      | otherwise         = z

improvedNoise :: Double -> Double -> Double -> Double
improvedNoise x' y' z' =
  -- Add blended results from 8 corners of cube.
  lerp w
    (lerp v
       (lerp u (grad (p ! aa)  x     y    z)
               (grad (p ! ba) (x-1)  y    z))
       (lerp u (grad (p ! ab)  x    (y-1) z)
               (grad (p ! bb) (x-1) (y-1) z)))
     (lerp v
       (lerp u (grad (p ! (aa+1))  x     y    (z-1))
               (grad (p ! (ba+1)) (x-1)  y    (z-1)))
       (lerp u (grad (p ! (ab+1))  x    (y-1) (z-1))
               (grad (p ! (bb+1)) (x-1) (y-1) (z-1))))
  where
    flr = realToFrac . floor
    -- Find unit cube thta contains point.
    (xx, yy, zz) = (floor x' .&. 255, floor y' .&. 255, floor z' .&. 255)
    -- Find relative x, y, z of point in cube.
    (x,  y,  z ) = (x' - flr x',      y' - flr y',      z' - flr z'     )
    -- Compute fade curves.
    (u,  v,  w ) = (fade x,           fade y,           fade z          )
    -- Hase coordinates of the 8 cube corners.
    (a,  aa, ab) = (p ! xx + yy,      p ! a + zz,       p ! (a+1) + zz  )
    (b,  ba, bb) = (p ! (xx+1) + yy,  p ! b + zz,       p ! (b+1) + zz  )

-------------------------------------------------------------------------------

fadeI :: Int -> Int
fadeI t = t0 + ((t .&. 255) * (t1 - t0) `shiftR` 8)
  where
    t0 = f ! (t `shiftR` 8)
    t1 = f ! (min 255 ((t `shiftR` 8) + 1))
    f = fromList $ map floor[(1e12) * fade (i / 256) | i <- [0..255]]

lerpI :: Int -> Int -> Int -> Int
lerpI t a b = a + shiftR (t * (b - a)) 12
  -- Add blended results from 8 corners of cube.

improvedNoiseI :: Int -> Int -> Int -> Int
improvedNoiseI x' y' z' =
  lerpI w
    (lerpI v
       (lerpI u (grad (p ! aa)  x     y    z)
               (grad (p ! ba) (x-n)  y    z))
       (lerpI u (grad (p ! ab)  x    (y-n) z)
               (grad (p ! bb) (x-n) (y-n) z)))
     (lerpI v
       (lerpI u (grad (p ! (aa+1))  x     y    (z-n))
               (grad (p ! (ba+1)) (x-n)  y    (z-n)))
       (lerpI u (grad (p ! (ab+1))  x    (y-n) (z-n))
               (grad (p ! (bb+1)) (x-n) (y-n) (z-n))))
  where
    n = shiftL 1 16
    (xx, yy, zz) = (shiftR x' 16 .&. 255, shiftR y' 16 .&. 255, shiftR z' 16 .&. 255)
    (x,  y,  z ) = (x' .&. (n-1),         y' .&. (n-1),         z' .&. (n-1)        )
    (u,  v,  w ) = (fadeI x,              fadeI y,              fadeI z             )
    (a,  aa, ab) = (p ! xx + yy,          p ! a + zz,           p ! (a+1) + zz  )
    (b,  ba, bb) = (p ! (xx+1) + yy,      p ! b + zz,           p ! (b+1) + zz  )
