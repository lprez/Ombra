{-# LANGUAGE RankNTypes, FlexibleContexts, MultiParamTypeClasses,
             FlexibleInstances #-}

module Utils.Reflex (
        playReflex,
        MonadInput(..),
        Key(..),
        MouseButton(..)
) where

import Data.Dependent.Map
import Control.Applicative
import Data.Functor.Identity
import Data.IORef
import Reflex
import Reflex.Host.Class
import Graphics.Rendering.Ombra.Draw
import Graphics.Rendering.Ombra.Shader (GVec4)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Fix
import Control.Monad.Reader
import Utils.Play
import Utils.Input

data Input t = Input { inputMousePosition :: Dynamic t (Float, Float)
                     , inputMouseUp :: MouseButton -> Event t ()
                     , inputMouseDown :: MouseButton -> Event t ()
                     , inputKeyUp :: Key -> Event t ()
                     , inputKeyDown :: Key -> Event t ()
                     , inputResize :: Event t (Int, Int)
                     }

class (Reflex t, MonadHold t m, MonadFix m, MonadSample t m) =>
        MonadInput t m where
        mousePosition :: m (Dynamic t (Float, Float))
        mouseUp :: MouseButton -> m (Event t ())
        mouseDown :: MouseButton -> m (Event t ())
        mouseButton :: MouseButton -> m (Dynamic t Bool)
        keyUp :: Key -> m (Event t ())
        keyDown :: Key -> m (Event t ())
        key :: Key -> m (Dynamic t Bool)
        resize :: m (Event t (Int, Int))

instance (Reflex t, MonadHold t m, MonadFix m, MonadSample t m) =>
        MonadInput t (ReaderT (Input t) m) where
        mousePosition = inputMousePosition <$> ask
        mouseUp b = ($ b) . inputMouseUp <$> ask
        mouseDown b = ($ b) . inputMouseDown <$> ask
        mouseButton b = do up <- mouseUp b
                           down <- mouseDown b
                           button up down
        keyUp k = ($ k) . inputKeyUp <$> ask
        keyDown k = ($ k) . inputKeyDown <$> ask
        key k = do up <- keyUp k
                   down <- keyDown k
                   button up down
        resize = inputResize <$> ask

button :: (Reflex t, MonadHold t m)
       => Event t ()
       -> Event t ()
       -> m (Dynamic t Bool)
button up down = holdDyn False $ leftmost [False <$ up, True <$ down]

selectKey :: (Reflex t, Eq a)
          => (b -> [a])
          -> InputTag a
          -> EventSelector t InputTag
          -> b
          -> Event t ()
selectKey convertKey tag (EventSelector select) key =
        fmap (const ()) . ffilter (flip elem $ convertKey key) $ select tag

playReflex :: Int
           -> Int
           -> Bool
           -> Draw GVec4 ()
           -> (forall t m. MonadInput t m
                        => Event t Float
                        -> m (Behavior t (Draw GVec4 ())))
           -> IO ()
playReflex w h requireBuffers initialization layerf = runSpiderHost $
        do timeRef <- liftIO $ newIORef 0
           (tickEvent, tickTriggerRef) <- newEventWithTriggerRef
           (inputEvent, inputTriggerRef) <- newEventWithTriggerRef
           (resizeEvent, resizeTriggerRef) <- newEventWithTriggerRef

           let inputSelector = fan $ fmap (fromList . (: [])) inputEvent
           pos <- holdDyn (0, 0) $ fmap fst (select inputSelector MouseMove)

           let i = Input { inputMousePosition = pos
                         , inputMouseUp = selectKey ((: []) . fromMouseButton)
                                                    MouseUp inputSelector
                         , inputMouseDown = selectKey ((: []) . fromMouseButton)
                                                      MouseDown inputSelector
                         , inputKeyUp = selectKey fromKey KeyUp inputSelector
                         , inputKeyDown = selectKey fromKey KeyDown inputSelector
                         , inputResize = resizeEvent
                         }
           currentLayer <- runHostFrame . flip runReaderT i $ layerf tickEvent
           
           play w h requireBuffers initialization
                (frame currentLayer timeRef tickTriggerRef)
                (handleInput inputTriggerRef)
                (handleResize resizeTriggerRef)

        where frame currentLayer timeRef tickTriggerRef time1 _ =
                do time0 <- liftIO . atomicModifyIORef timeRef $
                                                       \time0 -> (time1, time0)
                   let diff = if time0 == 0 then 0 else time1 - time0
                   liftIO . withTrigger tickTriggerRef $
                        \trigger -> fireEvents [ trigger :=> Identity diff ]
                   runHostFrame $ sample currentLayer

              handleInput inputTriggerRef ev = withTrigger inputTriggerRef $
                              \trigger -> fireEvents [ trigger :=> Identity ev ]

              handleResize resizeTriggerRef s = withTrigger resizeTriggerRef $
                              \trigger -> fireEvents [ trigger :=> Identity s ]

              withTrigger ref f = do mtrig <- readIORef ref
                                     case mtrig of
                                          Just trig -> runSpiderHost $ f trig
                                          Nothing -> return ()
