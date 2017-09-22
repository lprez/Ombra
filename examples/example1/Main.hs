{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Arrow
import Control.Applicative
import Data.Monoid (mconcat)
import Graphics.Rendering.Ombra
import Graphics.Rendering.Ombra.Blend (transparency, withBlendMode)
import Graphics.Rendering.Ombra.Vector

import Utils.Play (animation)
import Utils.Lighting
import Utils.Shapes (cube)
import Utils.Transform
import Utils.Vertex3D

main :: IO ()
main = animation render

render :: Float -> Draw GVec4 ()
render time = do draw $ scene viewMat
                 withBlendMode (Just transparency) $
                        drawBuffers 256 256
                                    (Right . byteGBuffer $ linearParams)
                                    (Right . depthBuffer $ nearestParams)
                                    (draw $ scene mirrorViewMat)
                                  $ \buf _ _ ->
                                          draw $ water buf viewMat light time
        where viewMat =     rotYMat4 (sin (time / 2) / 4)
                        .*. transMat4 (Vec3 0 0.5 (- 1.8))
              mirrorMat = reflectionMat4 (Vec4 0 1 0 (- 0.5))
              mirrorViewMat = mirrorMat .*. viewMat
              lightPos = Vec4 (cos time) (sin (time / 5)) 0 1
              light = Light { lightPosition = extract $ viewMat .* lightPos
                            , lightAmbient = Vec3 0.4 0.4 0.4
                            , lightDiffuse = Vec3 0.7 0.7 0.7
                            , lightAttenuation = Vec3 0 0.15 0.13
                            }
              scene viewMat = mconcat [ walls viewMat light
                                      , rotatingCube viewMat light time 0
                                      , rotatingCube viewMat light time 2.09
                                      , rotatingCube viewMat light time 4.19
                                      ]
              linearParams = parameters Linear Linear
              nearestParams = parameters Nearest Nearest

rotatingCube :: Mat4 -> Light -> Float -> Float -> Image GVec4
rotatingCube view light time off = image (vertexShader (pure time) ~~ view)
                                         (fragmentShader ~~ (light, specular))
                                         cube
        where transformMat time = let angle = time + off
                                      scale = scaleMat4 (Vec3 0.2 0.2 0.2)
                                      rot = rotYMat4 (time * 3)
                                      trans = transMat4 $ Vec3 (sin angle)
                                                               0.5
                                                               (cos angle)
                                  in trans .*. scale .*. rot
              vertexShader = ushader (\time -> mkVertexShader $
                                        transform ~* fmap transformMat time)
              fragmentShader = mkFragmentShader . arr $ \_ -> GVec4 0.8 0.2 0 1
              specular = Specular 4 1

walls :: Mat4 -> Light -> Image GVec4
walls viewMat light = image (vertexShader ~~ viewMat)
                            (fragmentShader ~~ (light, Specular 20 1))
                            cube
        where transformMat = scaleMat4 $ Vec3 2 2 2
              vertexShader = mkVertexShader $ transform ~~ transformMat
              fragmentShader = mkFragmentShader $ applyTexture ~~ tex

water :: GBuffer t GVec4 -> Mat4 -> Light -> Float -> Image GVec4
water buffer viewMat light time =
        image (vertexShader ~~ viewMat)
              (fragmentShader (pure (time * 2, buffer)) ~~ (light, specular))
              cube
        where transformMat =     transMat4 (Vec3 0 (-2) 0)
                             .*. scaleMat4 (Vec3 2.01 1 2.01)
              vertexShader = mkVertexShader $ transform ~~ transformMat
              fragmentShader = ushader $ mkFragmentShader . (applyWater ~*)
              specular = Specular 1 0.5

mkVertexShader :: VertexShader GVec4 GVec4
               -> VertexShader (GMat4, GVertex3D) (GVec4, GVertex3D)
mkVertexShader vs = shader $
            second (\(GVertex3D pos norm uv) -> ((pos ^| 1, norm ^| 0), uv))
        ^>> second (first $ vs *** vs)
        >>> (\(viewMat, ((pos, norm), uv)) ->
                ( viewMat .* pos
                , GVertex3D (extract $ viewMat .* pos)
                            (extract $ viewMat .* norm)
                            uv
                ))
        ^>> first (transform ~~ perspMat)
        where perspMat = perspectiveMat4 0.1 10000 90 1

mkFragmentShader :: FragmentShader GVertex3D GVec4
                 -> FragmentShader ((GLight, GSpecular), GVertex3D) GVec4
mkFragmentShader fs = shader $     second reverseNormal
                               >>> applyLight &&& (snd ^>> fs)
                               >>^ \(GVec3 lr lg lb, GVec4 r g b a) ->
                                       GVec4 (lr * r) (lg * g) (lb * b) a
        where applyLight =     (\((gLight, gSpec), (GVertex3D pos norm _)) ->
                                        (gLight, ((pos, norm), (1, gSpec))))
                           ^>> light


reverseNormal :: FragmentShader GVertex3D GVertex3D
reverseNormal = farr $ \fragment (GVertex3D pos norm uv) ->
                        let factor = ifB (fragFrontFacing fragment) 1 (-1)
                        in GVertex3D pos (factor *^ norm) uv

applyTexture :: FragmentShader (TextureSampler, GVertex3D) GVec4
applyTexture = sarr $ \(sampler, GVertex3D _ _ (GVec2 s t)) ->
                        sample sampler $ GVec2 s (1 - t)

applyWater :: FragmentShader ((GFloat, GBufferSampler t GVec4), GVertex3D) GVec4
applyWater = sarr $ \((time, sampler), GVertex3D _ _ (GVec2 s t)) ->
                        let n = (sin (t * 40 + time) + cos (s * 30 + time)) / 90
                            st' = GVec2 (s + n) (t + n)
                            GVec4 r g b _ = 0.5 *^ sampleGBuffer sampler st'
                        in GVec4 (r + 0.10) (g + 0.15) (b + 0.15) (0.6 + n * 7)

transform :: VertexShader (GMat4, GVec4) GVec4
transform = sarr $ \(matrix, vec) -> matrix .* vec

tex :: Texture
tex = mkTexture 128 128 (potLinear True) [cols]
        where cols = [ let (mx, my) = (mod x 16, mod y 16)
                       in if mx == 0 || my == 0 || mx == 15 || my == 15
                          then black
                          else visible (135 + mx * 8) (100 + mx * 8) (mx * 8)
                     | x <- [0 .. 127], y <- [0 .. 127]
                     ]
