module Main where

import Graphics.Rendering.Ombra.D3
import Common

scene :: Texture -> Float -> Layer
scene tex time = let rotatedCube off =   trans (Vec3 0 (- 0.4) (- 1.6))
                                       . rotY (time + off)
                                       . trans (Vec3 0 0 0.8)
                                       . rotY (time * 4)
                                       . scale 0.2
                                       $ cube tex

                     cubeGroup = viewPersp 0.5
                                           100000
                                           100
                                           idmtx
                                           [ rotatedCube 0
                                           , rotatedCube $ pi * 2 / 3
                                           , rotatedCube $ pi * 4 / 3 ]
                 in layerS cubeGroup

main :: IO ()
main = do t <- loadTexture "../tex.png"
          animation $ scene t
