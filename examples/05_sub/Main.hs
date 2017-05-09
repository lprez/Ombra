module Main where

import Graphics.Rendering.Ombra
import Graphics.Rendering.Ombra.D3
import Graphics.Rendering.Ombra.Vector
import ProgramCommon
import Program3D
import qualified Graphics.Rendering.Ombra.D2 as D2
import qualified ProgramDistort as D2
import Utils.TextureLoader
import Utils.Play

scene :: Texture -> Texture -> Float -> Layer
scene tex noiseTex time =
        let rotatedCube off =   trans (Vec3 0 (- 0.4) (- 1.6))
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

            group =     Time -= time / 2
                    :~> withTexture noiseTex (NoiseTexture -=)
                    :~> viewGroup
            sceneLayer = layer prg group

            -- A rectangle with a texture that will be distorted.
            distortedRect tex =     Time -= time / 8
                                :~> withTexture noiseTex (NoiseTexture -=)
                                :~> D2.scale 2 (D2.rect tex)

            -- We use the 'subLayer' function to draw the 3D scene (the one of
            -- the previous example) to a 512x512 texture, and pass it to
            -- a 2D layer, which draws a distorted texture.
            in subLayer 512 512 sceneLayer $ \renderedSceneTex ->
                        layer prg2D
                      . D2.view idmtx
                      $ [distortedRect renderedSceneTex]
        where prg :: Program Uniforms Geometry3D
              prg = program vertexShader fragmentShader

              prg2D :: Program D2.Uniforms D2.Geometry2D
              prg2D = program D2.vertexShader D2.fragmentShader

main :: IO ()
main = do t <- loadTexture "../tex.png"
          noise <- loadTexture "../clouds.png"
          animation $ scene t noise
