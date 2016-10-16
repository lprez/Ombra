{-# LANGUAGE DataKinds, RebindableSyntax, DeriveGeneric, GADTs #-}

-- A program consists of a vertex shader and a fragment shader.
module Program (
        module Graphics.Rendering.Ombra.Shader.Default3D,
        fragmentShader
) where

import Graphics.Rendering.Ombra.Shader

-- This module contains the standard shaders (the ones you get with 'layerS').
-- In this case we only need to modify the fragment shader, and also don't need
-- to add or change the uniforms and attributes.
import Graphics.Rendering.Ombra.Shader.Default3D hiding (fragmentShader)

-- The fragment shader operates on the colors of the pixels.
fragmentShader ::
        FragmentShader '[ Texture2 ]               -- List of uniforms
                                                   -- (i.e. values we can send
                                                   -- to the GPU and are
                                                   -- constant for every
                                                   -- shader call).
                       '[ Position3, UV, Normal3 ] -- List of inputs (values
                                                   -- the fragment shader
                                                   -- receives from the vertex
                                                   -- one and change for every
                                                   -- vertex/fragment).

-- We define the fragment shader as if it was a function from an heterogeneous
-- list of uniforms and an heterogeneous list of attribues to an heterogeneous
-- list of colors (normally there is only one color, but there may be more when
-- you're drawing to multiple textures instead of the screen). :- is the cons
-- operator and 'N' marks the end of the list.
fragmentShader (Texture2 sampler :- N)  -- The texture of the object to draw.
               (Position3 _ :-   -- The world coordinates of the pixel. We
                                 -- don't need them, but we add it to match the
                                 -- output of the standard vertex shader.
                UV (Vec2 s t) :- -- The UV coordinates of the current fragment,
                                 -- we use them to get the right pixel from the
                                 -- texture.
                _)
                = let (Vec4 x y z _) = fragCoord -- The coordinates of the
                                                 -- current pixel, we use them
                                                 -- to vary the color of the
                                                 -- effect.
                      effectOpacity = abs $ sin (s * 6 + t * 2 + z * 10) / 2
                                          + cos (t * 7 - s * 5 + z * 10)
                      effectColor = normalize $ Vec4 x y (x - y) 1
                      texColor = texture2D sampler $ Vec2 s (1 - t)
                  in Fragment (mix texColor effectColor effectOpacity) :- N
