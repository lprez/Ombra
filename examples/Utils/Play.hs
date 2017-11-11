{-# LANGUAGE CPP, DataKinds #-}

#ifdef __GHCJS__
{-# LANGUAGE OverloadedStrings, JavaScriptFFI, InterruptibleFFI #-}
#endif

module Utils.Play (
        play,
        animation,
        static
) where

import Graphics.Rendering.Ombra.Backend
import Graphics.Rendering.Ombra.Draw
import Graphics.Rendering.Ombra.Shader (GVec4)
import Control.Monad.IO.Class
import Data.IORef
import System.Exit (exitFailure)

import Utils.Input

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

foreign import javascript unsafe "$1.width" getWidth :: JSVal -> IO Int
foreign import javascript unsafe "$1.height" getHeight :: JSVal -> IO Int

foreign import javascript unsafe "$1.data" imgData :: JSVal -> IO Uint8Array

foreign import javascript interruptible "window.requestAnimationFrame($c);"
        waitFrame :: IO Double

foreign import javascript unsafe "alert" alert :: JSString -> IO ()

foreign import javascript unsafe "$1.addEventListener($2, $3)"
        addEventListener :: JSVal -> JSString -> Callback (JSVal -> IO ()) -> IO ()
foreign import javascript unsafe "$1.clientX" clientX :: JSVal -> IO Int
foreign import javascript unsafe "$1.clientY" clientY :: JSVal -> IO Int

play :: Int
     -> Int
     -> Bool
     -> Draw GVec4 a
     -> (a -> Float -> (Int, Int) -> Draw GVec4 ())
     -> (InputEvent -> IO ())
     -> ((Int, Int) -> IO ())
     -> IO ()
play width height requireExts initialization frame inpCallback _ =
        do canvas <- query "canvas"
           ctx <- makeContext canvas
           width <- getWidth canvas
           height <- getHeight canvas
           inpCtl <- mkInputControl canvas inpCallback

           mextError <- checkExtensions requireExts ctx
           case mextError of
                Just extError -> alert (fromString extError) >> exitFailure
                Nothing -> return ()

           runDraw width height ctx $
                do inval <- initialization
                   loop $ \t -> do clearColor >> clearDepth
                                   frame inval (realToFrac t) (width, height)
        where loop a = do t <- liftIO waitFrame
                          a $ t / 1000
                          loop a


#else

import Control.Concurrent
import Data.Time.Clock
import Graphics.Rendering.Ombra.Backend.OpenGL
import Graphics.UI.GLFW as G
import System.Mem (performMinorGC)

play :: Int
     -> Int
     -> Bool
     -> Draw GVec4 a
     -> (a -> Float -> (Int, Int) -> Draw GVec4 ())
     -> (InputEvent -> IO ())
     -> ((Int, Int) -> IO ())
     -> IO ()
play width height requireExts initialize frame inpCallback sizeCallback =
        do w <- initWindow
           ctx <- makeContext
           t0 <- getCurrentTime
           inpCtl <- mkInputControl w inpCallback
           sizeRef <- newIORef Nothing

           mextError <- checkExtensions requireExts ctx
           case mextError of
                Just extError -> putStrLn extError >> exitFailure
                Nothing -> return ()

           setWindowSizeCallback w . Just $ \_ width' height' ->
                   do writeIORef sizeRef $ Just (width', height')
                      sizeCallback (width', height')

           runDraw width height ctx $ initialize >>= \inval -> loop t0 $ \t ->
                do clearColor >> clearDepth
                   resized <- liftIO . atomicModifyIORef sizeRef $ (,) Nothing
                   case resized of
                        Just size -> do resizeViewport (0, 0) size
                                        frame inval t size
                        Nothing -> do (width, height) <- liftIO $ getWindowSize w
                                      frame inval t (width, height)

                   (width', height') <- liftIO $ getWindowSize w
                   liftIO $ performMinorGC >> swapBuffers w
        where loop t0 a = do t <- liftIO $ getCurrentTime
                             a . realToFrac $ diffUTCTime t t0
                             tf <- liftIO $ getCurrentTime
                             let diff = realToFrac $ diffUTCTime tf t
                                 delaySec = max 0 $ maxDelay - diff
                                 delay = floor $ delaySec * 1000000
                             liftIO pollEvents
                             liftIO . threadDelay $ delay
                             loop t0 a

              maxDelay = 0.03 :: Float

              initWindow = do G.init
                              windowHint $ WindowHint'ClientAPI ClientAPI'OpenGL
                              windowHint $ WindowHint'ContextVersionMajor 2
                              windowHint $ WindowHint'ContextVersionMinor 1
                              windowHint $ WindowHint'StencilBits 8
                              Just w <- createWindow width height ""
                                                     Nothing Nothing
                              makeContextCurrent $ Just w
                              return w
#endif

checkExtensions :: Bool -> Ctx -> IO (Maybe String)
checkExtensions requireAllExtensions ctx =
        do vaoExt <- hasVertexArrayObjects ctx
           floatTexExt <- hasFloatTextures ctx
           drawBufsExt <- hasDrawBuffers ctx
           derivativesExt <- hasStandardDerivatives ctx

           let e1 = [ "\nVertex array objects are not supported." | not vaoExt ]
               e2 = [ "\nFloat textures are not supported."
                    | requireAllExtensions && not floatTexExt ]
               e3 = [ "\nMRT are not supported."
                    | requireAllExtensions && not drawBufsExt ]
               e4 = [ "\nStandard derivatives are not supported."
                    | requireAllExtensions && not derivativesExt ]
           
           return $ case concat [e1, e2, e3, e4] of
                         [] -> Nothing
                         errs -> Just $ "ERROR:" ++ concat errs

animation :: Draw GVec4 a -> (a -> Float -> Draw GVec4 ()) -> IO ()
animation init f = play 512 512
                        False
                        init
                        (\x t _ -> f x t)
                        (const (return ()))
                        (const (return ()))

static :: Draw GVec4 () -> IO ()
static = flip animation $ \_ _ -> return ()
