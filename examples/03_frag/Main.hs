module Main where

import Graphics.Rendering.Ombra
import Graphics.Rendering.Ombra.D3
import Program
import Common

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
                 -- This time we use 'layer' instead of 'layerS' because we're
                 -- using a custom program.
                 in layer prg viewGroup
        where -- Creating programs is a slow operation so we put it here to
              -- avoid creating one in every frame.
              prg :: Program Uniforms Attributes
              prg = program vertexShader fragmentShader

main :: IO ()
main = do t <- loadTexture "../tex.png"
          animation $ scene t
