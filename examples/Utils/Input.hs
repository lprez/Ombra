{-# LANGUAGE CPP, OverloadedStrings, GADTs #-}

#ifdef __GHCJS__
{-# LANGUAGE JavaScriptFFI #-}
#endif

module Utils.Input (
        InputControl,
        InputEvent,
        InputTag(..),
        Key(..),
        MouseButton(..),
        mkInputControl,
        lockCursor,
        unlockCursor,
        fromKey,
        fromMouseButton
) where

import Control.Applicative
import Control.Monad (when)
import Control.Monad.Trans.State
import Data.Dependent.Sum
import Data.Functor.Identity
import Data.GADT.Compare
import Data.IORef

import Utils.Key

#ifdef __GHCJS__

import GHCJS.Foreign hiding (Object)
import GHCJS.Foreign.Callback
import GHCJS.Marshal
import GHCJS.Types
import JavaScript.Object.Internal

#else

import Graphics.UI.GLFW hiding (Key, MouseButton)

#endif

cursorCoordinates :: (Float, Float) -> (Float, Float) -> Float -> (Float, Float)
cursorCoordinates (x, y) (w, h) off = (x * 2 / w + off, -y * 2 / h - off)


data InputTag a where
        KeyDown :: InputTag RawKey
        KeyUp :: InputTag RawKey
        MouseDown :: InputTag RawMouseButton
        MouseUp :: InputTag RawMouseButton
        MouseMove :: InputTag ((Float, Float), (Float, Float))
        PointerLockChange :: InputTag Bool
                -- | Wheel (Int, Int)

type InputEvent = DSum InputTag Identity

instance GEq InputTag where
        geq KeyDown KeyDown = Just Refl
        geq KeyUp KeyUp = Just Refl
        geq MouseDown MouseDown = Just Refl
        geq MouseUp MouseUp = Just Refl
        geq MouseMove MouseMove = Just Refl
        geq PointerLockChange PointerLockChange = Just Refl
        geq _ _ = Nothing

instance GCompare InputTag where
        gcompare KeyDown KeyDown = GEQ
        gcompare KeyDown _ = GLT
        gcompare KeyUp KeyDown = GGT
        gcompare KeyUp KeyUp = GEQ
        gcompare KeyUp _ = GLT
        gcompare MouseDown KeyDown = GGT
        gcompare MouseDown KeyUp = GGT
        gcompare MouseDown MouseDown = GEQ
        gcompare MouseDown _ = GLT
        gcompare MouseUp KeyDown = GGT
        gcompare MouseUp KeyUp = GGT
        gcompare MouseUp MouseDown = GGT
        gcompare MouseUp MouseUp = GEQ
        gcompare MouseUp _ = GLT
        gcompare MouseMove PointerLockChange = GLT
        gcompare MouseMove MouseMove = GEQ
        gcompare MouseMove _ = GGT
        gcompare PointerLockChange PointerLockChange = GEQ
        gcompare PointerLockChange _ = GGT

#ifdef __GHCJS__

data InputControl = JSInputControl (IORef Bool) JSVal

-- | Create a 'InputControl' from a DOM element.
mkInputControl :: JSVal -> (InputEvent -> IO ()) -> IO (Maybe InputControl)
mkInputControl element _ | isNull element = return Nothing
mkInputControl element eventCallback =
        do ptrLockVar <- newIORef False
           doc <- document
           event element "keydown" eventKeyDown
           event element "keyup" eventKeyUp
           event element "mousedown" eventMouseDown
           event element "mouseup" eventMouseUp
           event element "mousemove" $ eventMouseMove element
           -- event element "wheel" eventWheel
           event doc "pointerlockchange" $
                const (eventPointerLockChange element)
           event doc "mozpointerlockchange" $
                const (eventPointerLockChange element)
           ptrLockPerformer element ptrLockVar "mouseup"
           ptrLockPerformer element ptrLockVar "keyup"
           return . Just $ JSInputControl ptrLockVar element

        where event element name getEvent =
                do callback <- asyncCallback1 $ \rawEvent ->
                                getEvent (Object rawEvent) >>= eventCallback
                   addEventListener element name callback

              ptrLockPerformer element ptrLockVar name =
                do callback <- syncCallback1 ContinueAsync $ \_ -> do
                                lock <- atomicModifyIORef' ptrLockVar
                                                           (\x -> (False, x))
                                when lock $ lockCursorRaw element
                   addEventListener element name callback


lockCursor :: InputControl -> IO ()
lockCursor (JSInputControl ptrLockVar _) = writeIORef ptrLockVar True

unlockCursor :: InputControl -> IO ()
unlockCursor (JSInputControl ptrLockVar _) = writeIORef ptrLockVar False
                                               >> unlockCursorRaw

eventKeyDown :: Object -> IO InputEvent
eventKeyDown ev = ((KeyDown :=>) . Identity) <$> prop "keyCode" ev

eventKeyUp :: Object -> IO InputEvent
eventKeyUp ev = ((KeyUp :=>) . Identity) <$> prop "keyCode" ev

eventMouseDown :: Object -> IO InputEvent
eventMouseDown ev = ((MouseDown :=>) . Identity) <$> prop "button" ev

eventMouseUp :: Object -> IO InputEvent
eventMouseUp ev = ((MouseUp :=>) . Identity) <$> prop "button" ev

eventMouseMove :: JSVal -> Object -> IO InputEvent
eventMouseMove elem ev@(Object evVal) =
        do width <- fi <$> prop "clientWidth" (Object elem)
           height <- fi <$> prop "clientHeight" (Object elem)
           clientX <- fi <$> prop "clientX" ev
           clientY <- fi <$> prop "clientY" ev
           moveX <- fi <$> movementX evVal
           moveY <- fi <$> movementY evVal
           pure $ MouseMove :=> Identity ( cursorCoordinates (clientX, clientY)
                                                             (width, height)
                                                             (- 1)
                                         , cursorCoordinates (moveX, moveY)
                                                             (width, height)
                                                             0
                                         )
        where fi = fromIntegral :: Int -> Float

{-
eventWheel :: Object -> IO InputEvent
eventWheel ev = Wheel <$> ((,) <$> prop "deltaX" ev
                               <*> prop "deltaY" ev)
-}

eventPointerLockChange :: JSVal -> IO InputEvent
eventPointerLockChange el = fmap ((PointerLockChange :=>) . Identity)
                                 (isPointerLockElement el)

prop :: FromJSVal a => JSString -> Object -> IO a
prop s o = unsafeGetProp s o >>= fromJSValUnchecked

foreign import javascript unsafe "$1.addEventListener($2, $3)"
        addEventListener :: JSVal -> JSString -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe " if ($1.requestPointerLock)\
                                       $1.requestPointerLock()\
                                   else\
                                       $1.mozRequestPointerLock()"
        lockCursorRaw :: JSVal -> IO ()

foreign import javascript unsafe " if (document.exitPointerLock)\
                                       document.exitPointerLock()\
                                   else\
                                       document.mozExitPointerLock()"
        unlockCursorRaw :: IO ()

foreign import javascript unsafe "$1.style.cursor = 'none'"
        hideCursor :: JSVal -> IO ()

foreign import javascript unsafe "$1.style.cursor = 'auto'"
        showCursor :: JSVal -> IO ()

foreign import javascript unsafe " document.pointerLockElement === $1 ||\
                                   document.mozPointerLockElement === $1"
        isPointerLockElement :: JSVal -> IO Bool

foreign import javascript unsafe "$r = document" document :: IO JSVal

foreign import javascript unsafe "$1.movementX || $1.mozMovementX || 0"
        movementX :: JSVal -> IO Int

foreign import javascript unsafe "$1.movementY || $1.mozMovementY || 0"
        movementY :: JSVal -> IO Int

#else

type InputCallback = InputEvent -> IO ()

data InputControl = GLFWInputControl Window (InputCallback)

mkInputControl :: Window -> InputCallback -> IO (Maybe InputControl)
mkInputControl w callback =
        do setKeyCallback w (Just $ const (keyEvent callback))
           setMouseButtonCallback w (Just $ const (mouseButtonEvent callback))
           setCursorPosCallback w (Just $ cursorPosEvent callback)
           return . Just $ GLFWInputControl w callback

lockCursor :: InputControl -> IO ()
lockCursor (GLFWInputControl w callback) =
        do setCursorInputMode w CursorInputMode'Hidden
           callback $ PointerLockChange :=> Identity True

unlockCursor :: InputControl -> IO ()
unlockCursor (GLFWInputControl w callback) =
        do setCursorInputMode w CursorInputMode'Normal
           callback $ PointerLockChange :=> Identity False

keyEvent :: InputCallback -> RawKey -> Int -> KeyState -> ModifierKeys -> IO ()
keyEvent callback key _ KeyState'Pressed _ = callback $ KeyDown :=> Identity key
keyEvent callback key _ KeyState'Released _ = callback $ KeyUp :=> Identity key
keyEvent _ _ _ _ _ = return ()

mouseButtonEvent :: InputCallback
                 -> RawMouseButton
                 -> MouseButtonState 
                 -> ModifierKeys
                 -> IO ()
mouseButtonEvent callback button MouseButtonState'Pressed _ =
        callback $ MouseDown :=> Identity button
mouseButtonEvent callback button MouseButtonState'Released _ =
        callback $ MouseUp :=> Identity button

cursorPosEvent :: InputCallback -> Window -> Double -> Double -> IO ()
cursorPosEvent callback w dx dy = 
        do (width, height) <- getWindowSize w
           let fsize = (fromIntegral width, fromIntegral height)
               fpos = (realToFrac dx, realToFrac dy)
               coords = cursorCoordinates fpos fsize (-1)
           callback $ MouseMove :=> Identity (coords, coords)

#endif
