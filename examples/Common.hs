{-# LANGUAGE CPP, DataKinds #-}

#ifdef __GHCJS__
{-# LANGUAGE OverloadedStrings, JavaScriptFFI, InterruptibleFFI #-}
#endif

module Common (
        play,
        animation,
        animation',
        static,
        loadTexture,
#ifdef __GHCJS__
        query,
        waitFrame
#endif
) where

import Graphics.Rendering.Ombra
import qualified Graphics.Rendering.Ombra.D2 as D2
import Graphics.Rendering.Ombra.Draw
import Control.Monad.IO.Class
import Data.Hashable
import Data.IORef
import System.Exit (exitFailure)

#ifdef __GHCJS__

import Data.String
import Graphics.Rendering.Ombra.Backend.WebGL
import GHCJS.Foreign.Callback
import GHCJS.Types
import JavaScript.TypedArray
import JavaScript.TypedArray.Internal
import JavaScript.TypedArray.DataView
import System.Mem (performGC)

foreign import javascript unsafe "document.querySelector($1)"
        query :: JSString -> IO JSVal

foreign import javascript interruptible
        "var img = new Image();                                         \
         img.src = $1;                                                  \
         img.onload = function () {                                     \
                 var canvas = document.createElement('canvas'),         \
                     ctx = canvas.getContext('2d');                     \
                 canvas.width = img.width;                              \
                 canvas.height = img.height;                            \
                 ctx.drawImage(img, 0, 0);                              \
                 var imgData = ctx.getImageData( 0, 0, img.width        \
                                               , img.height);           \
                 $c({ data: new Uint8Array(imgData.data.buffer)         \
                    , width: imgData.width, height: imgData.height });  \
          };                                                            "
        loadImage :: JSString -> IO JSVal

foreign import javascript unsafe "$1.width" imgWidth :: JSVal -> IO Int
foreign import javascript unsafe "$1.height" imgHeight :: JSVal -> IO Int
foreign import javascript unsafe "$1.data" imgData :: JSVal -> IO Uint8Array

foreign import javascript interruptible "window.requestAnimationFrame($c);"
        waitFrame :: IO Double

foreign import javascript unsafe "alert" alert :: JSString -> IO ()

foreign import javascript unsafe "$1.addEventListener($2, $3)"
        addEventListener :: JSVal -> JSString -> Callback (JSVal -> IO ()) -> IO ()
foreign import javascript unsafe "$1.clientX" clientX :: JSVal -> IO Int
foreign import javascript unsafe "$1.clientY" clientY :: JSVal -> IO Int

play :: MonadIO m
     => Bool
     -> Draw ()
     -> (Float -> (Int, Int) -> m (Layer' Drawable t a))
     -> (a -> m ())
     -> m ()
play requireBuffers initialization getLayer layerRetf =
        do cposRef <- liftIO $ newIORef (0, 0)
           canvas <- liftIO $ query "canvas"
           ctx <- liftIO $ makeContext canvas

           mextError <- liftIO $ checkExtensions requireBuffers ctx
           case mextError of
                Just extError -> liftIO $ alert (fromString extError) >>
                                          exitFailure
                Nothing -> return ()

           liftIO $ do callback <- asyncCallback1 $
                                \ev -> ((,) <$> clientX ev <*> clientY ev) >>=
                                        writeIORef cposRef
                       addEventListener canvas "mousemove" callback
           stateRef <- liftIO $ drawState 512 512 >>= newIORef

           let liftDraw = liftIO . flip (refDrawCtx ctx) stateRef
           liftDraw $ drawInit >> initialization
           loop $ \t -> do cpos <- liftIO $ readIORef cposRef
                           layer <- getLayer (realToFrac t) cpos
                           layerRet <- liftDraw $
                                do clearBuffers [ColorBuffer, DepthBuffer]
                                   drawLayer layer
                           layerRetf layerRet
                           liftIO $ performGC

           return ()
        where loop a = do t <- liftIO waitFrame
                          a $ t / 1000
                          loop a

loadTexture :: FilePath -> IO Texture
loadTexture path = do img <- loadImage $ fromString path
                      w <- imgWidth img
                      h <- imgHeight img
                      arr <- imgData img

                      let buf = buffer arr
                          dw = dataView buf
                          l = w * h * 4
                          pxhash = flip hashWithSalt l $
                                        foldl (\s i -> hashWithSalt s $
                                                        getUint8 i dw)
                                        0x36d1615b7400a4
                                        [ 0 .. l - 1 ]

                      return . mkTextureRaw w h True [arr] $ hash (w, h, pxhash)

#else

import Codec.Picture
import Codec.Picture.Types (promoteImage)
import Control.Concurrent
import qualified Data.Vector.Storable as V
import Data.Time.Clock
import Graphics.Rendering.Ombra.Backend.OpenGL
import Graphics.UI.GLFW hiding (Image)
import qualified Graphics.UI.GLFW as G
import System.Mem (performMinorGC)

play :: MonadIO m
     => Bool
     -> Draw ()
     -> (Float -> (Int, Int) -> m (Layer' Drawable t a))
     -> (a -> m ())
     -> m ()
play requireBuffers initialization getLayer layerRetf =
        do w <- liftIO $ initWindow
           stateRef <- liftIO $ drawState 512 512 >>= newIORef
           ctx <- liftIO $ makeContext
           t0 <- liftIO $ getCurrentTime

           mextError <- liftIO $ checkExtensions requireBuffers ctx
           case mextError of
                Just extError -> liftIO $ putStrLn extError >> exitFailure
                Nothing -> return ()

           let liftDraw = liftIO . flip (refDrawCtx ctx) stateRef
           liftDraw $ drawInit >> initialization
           loop t0 t0 $ \n -> do (x, y) <- liftIO $ getCursorPos w
                                 layer <- getLayer n (floor x, floor y)
                                 layerRet <- liftDraw $
                                   do clearBuffers [ColorBuffer, DepthBuffer]
                                      drawLayer layer
                                 layerRetf layerRet
                                 liftIO $ do performMinorGC
                                             swapBuffers w
           return ()
        where loop t0 tp a = do t <- liftIO $ getCurrentTime
                                let dt = diffUTCTime t tp
                                    t' = diffUTCTime t t0
                                a $ realToFrac t'
                                liftIO $ do print $ 1 / dt
                                            threadDelay delay
                                loop t0 t a

              delay = 30000

              initWindow = do G.init
                              windowHint $ WindowHint'ClientAPI ClientAPI'OpenGL
                              windowHint $ WindowHint'ContextVersionMajor 2
                              windowHint $ WindowHint'ContextVersionMinor 1
                              windowHint $ WindowHint'StencilBits 8
                              Just w <- createWindow 512 512 "" Nothing Nothing
                              makeContextCurrent $ Just w
                              return w

loadTexture :: FilePath -> IO Texture
loadTexture path = do eimg <- readImage path
                      case eimg of
                           Left err -> error err
                           Right img ->
                                   case convertRGBA8 img of
                                        {-
                                        Image w h v ->
                                            let (fp, l) = unsafeToForeignPtr0 v
                                                hash = hash ...
                                            in mkTextureRaw ...
                                        -}
                                        Image w h v -> return .
                                                        mkTexture w h True
                                                                $ [colList v]
        where colList = fst . V.foldr (\x (l, cs) ->
                                        case cs of
                                             [g, b, a] -> ( Color x g b a : l
                                                          , [] )
                                             _ -> (l, x : cs)
                                      )
                                      ([], [])
#endif

checkExtensions :: GLES => Bool -> Ctx -> IO (Maybe String)
checkExtensions requireBuffers ctx =
        do vaoExt <- hasVertexArrayObjects ctx
           floatTexExt <- hasFloatTextures ctx
           drawBufsExt <- hasDrawBuffers ctx

           let e1 = [ "\nVertex array objects are not supported." | not vaoExt ]
               e2 = [ "\nFloat textures are not supported."
                    | requireBuffers && not floatTexExt ]
               e3 = [ "\nMRT are not supported."
                    | requireBuffers && not drawBufsExt ]
           
           return $ case concat [e1, e2, e3] of
                         [] -> Nothing
                         errs -> Just $ "ERROR:" ++ concat errs

animation' :: Bool -> Draw () -> (Float -> Layer) -> IO ()
animation' ext init f = play ext init (\t _ -> return $ f t) (const $ return ()) 

animation :: (Float -> Layer) -> IO ()
animation = animation' False $ return ()

static :: Layer -> IO ()
static = animation . const
