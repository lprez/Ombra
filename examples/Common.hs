module Common (
        animation,
        static,
        loadTexture
) where

import Codec.Picture
import Codec.Picture.Types (promoteImage)
import Control.Concurrent
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.Hashable
import qualified Data.Vector.Storable as V
import Graphics.Rendering.Ombra.Generic
import qualified Graphics.Rendering.Ombra.D2 as D2
import Graphics.Rendering.Ombra.Draw
import Graphics.Rendering.Ombra.Backend.OpenGL
import Graphics.Rendering.Ombra.Texture (mkTexture)
import Graphics.UI.GLFW as G

animation :: (Float -> Layer) -> IO ()
animation layer =
        do G.init
           windowHint $ WindowHint'ClientAPI ClientAPI'OpenGL
           windowHint $ WindowHint'ContextVersionMajor 2
           windowHint $ WindowHint'ContextVersionMinor 1
           mw@(Just w) <- createWindow 512 512 "" Nothing Nothing
           makeContextCurrent mw
           stateRef <- drawState 512 512 >>= newIORef
           ctx <- makeContext
           flip (refDrawCtx ctx) stateRef $
                do drawInit
                   loop 0 $ \n -> do
                         drawBegin
                         drawLayer $ layer n
                         drawEnd
                         liftIO $ swapBuffers w
           return ()
        where loop n a = do a n
                            liftIO $ threadDelay delay
                            loop (n + fromIntegral delay / 1000000) a
              delay = 30000

static :: Layer -> IO ()
static = animation . const

loadTexture :: FilePath -> IO Texture
loadTexture path = do eimg <- readImage path
                      case eimg of
                           Left err -> error err
                           Right img ->
                                   case convertRGBA8 img of
                                        Image w h v -> return . mkTexture w h $
                                                        colList v
        where {-
              convert :: DynamicImage -> Image PixelRGBA8
              convert (ImageRGBA8 img) = img
              convert (ImageRGB8 img) = promoteImage img
              convert (ImageYA8 img) = promoteImage img
              convert (ImageY8 img) = promoteImage img
              convert _ = error "Unsupported image format."
              -}

              colList = fst . V.foldr (\x (l, cs) ->
                                        case cs of
                                             [g, b, a] -> ( Color x g b a : l
                                                          , [] )
                                             _ -> (l, x : cs)
                                      )
                                      ([], [])
