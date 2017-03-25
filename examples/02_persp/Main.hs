module Main where

import Graphics.Rendering.Ombra
import Graphics.Rendering.Ombra.D3
import Graphics.Rendering.Ombra.Vector
import Common

-- This is similiar to the scene of the previous example, but with perspective
-- projection and three objects instead of one.
scene :: Texture -> Float -> Layer
scene tex time = let rotatedCube off =   trans (Vec3 0 (- 0.4) (- 1.6))
                                       . rotY (time + off)
                                       . trans (Vec3 0 0 0.8)
                                       . rotY (time * 4)
                                       . scale 0.2
                                       $ cube tex

                     viewGroup = viewPersp 0.5
                                           100000
                                           100
                                           idmtx
                                           [ rotatedCube 0
                                           , rotatedCube $ pi * 2 / 3
                                           , rotatedCube $ pi * 4 / 3 ]
                 in layerS viewGroup

main :: IO ()
main = do t <- loadTexture "../tex.png"
          animation $ scene t
