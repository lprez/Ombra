{-# LANGUAGE CPP #-}

module Utils.Key (
        Key(..),
        RawKey,
        MouseButton(..),
        RawMouseButton,
        fromKey,
        fromMouseButton
) where

#ifdef __GHCJS__
#else
import qualified Graphics.UI.GLFW as GLFW
#endif

data Key = KeyA | KeyB | KeyC | KeyD | KeyE | KeyF | KeyG | KeyH | KeyI | KeyJ
         | KeyK | KeyL | KeyM | KeyN | KeyO | KeyP | KeyQ | KeyR | KeyS | KeyT
         | KeyU | KeyV | KeyW | KeyX | KeyY | KeyZ | Key0 | Key1 | Key2 | Key3
         | Key4 | Key5 | Key6 | Key7 | Key8 | Key9 | KeySpace | KeyEnter
         | KeyTab | KeyEsc | KeyBackspace | KeyShift | KeyControl | KeyAlt
         | KeyCapsLock | KeyNumLock | KeyArrowLeft | KeyArrowUp | KeyArrowRight
         | KeyArrowDown | KeyIns | KeyDel | KeyHome | KeyEnd | KeyPgUp
         | KeyPgDown | KeyF1 | KeyF2 | KeyF3 | KeyF4 | KeyF5 | KeyF6 | KeyF7
         | KeyF8 | KeyF9 | KeyF10 | KeyF11 | KeyF12
         -- | KeyPadDel | KeyPadIns | KeyPadEnd | KeyPadDown | KeyPadPgDown
         -- | KeyPadLeft | KeyPadRight | KeyPadHome | KeyPadUp | KeyPadPgUp
         | KeyPadAdd | KeyPadSub | KeyPadMul | KeyPadDiv | KeyPadEnter
         | KeyPadDot | KeyPad0 | KeyPad1 | KeyPad2 | KeyPad3 | KeyPad4
         | KeyPad5 | KeyPad6 | KeyPad7 | KeyPad8 | KeyPad9
         deriving (Eq, Show)

data MouseButton = MouseLeft | MouseMiddle | MouseRight deriving (Eq, Show)

#ifdef __GHCJS__

type RawKey = Int
type RawMouseButton = Int

fromKey :: Key -> [RawKey]
fromKey KeyA = [65, 97]
fromKey KeyB = [66, 98]
fromKey KeyC = [67, 99]
fromKey KeyD = [100, 68]
fromKey KeyE = [101, 69]
fromKey KeyF = [102, 70]
fromKey KeyG = [103, 71]
fromKey KeyH = [104, 72]
fromKey KeyI = [105, 73]
fromKey KeyJ = [106, 74]
fromKey KeyK = [107, 75]
fromKey KeyL = [108, 76]
fromKey KeyM = [109, 77]
fromKey KeyN = [110, 78]
fromKey KeyO = [111, 79]
fromKey KeyP = [112, 80]
fromKey KeyQ = [113, 81]
fromKey KeyR = [114, 82]
fromKey KeyS = [115, 83]
fromKey KeyT = [116, 84]
fromKey KeyU = [117, 85]
fromKey KeyV = [118, 86]
fromKey KeyW = [119, 87]
fromKey KeyX = [120, 88]
fromKey KeyY = [121, 89]
fromKey KeyZ = [122, 90]
fromKey Key0 = [48]
fromKey Key1 = [49]
fromKey Key2 = [50]
fromKey Key3 = [51]
fromKey Key4 = [52]
fromKey Key5 = [53]
fromKey Key6 = [54]
fromKey Key7 = [55]
fromKey Key8 = [56]
fromKey Key9 = [57]
fromKey KeySpace = [32]
fromKey KeyEnter = [13]
fromKey KeyTab = [9]
fromKey KeyEsc = [27]
fromKey KeyBackspace = [8]
fromKey KeyShift = [16]
fromKey KeyControl = [17]
fromKey KeyAlt = [18]
fromKey KeyCapsLock = [20]
fromKey KeyNumLock = [144]
fromKey KeyArrowLeft = [37]
fromKey KeyArrowUp = [38]
fromKey KeyArrowRight = [39]
fromKey KeyArrowDown = [40]
fromKey KeyIns = [45]
fromKey KeyDel = [46]
fromKey KeyHome = [36]
fromKey KeyEnd = [35]
fromKey KeyPgUp = [33]
fromKey KeyPgDown = [34]
fromKey KeyF1 = [112]
fromKey KeyF2 = [113]
fromKey KeyF3 = [114]
fromKey KeyF4 = [115]
fromKey KeyF5 = [116]
fromKey KeyF6 = [117]
fromKey KeyF7 = [118]
fromKey KeyF8 = [119]
fromKey KeyF9 = [120]
fromKey KeyF10 = [121]
fromKey KeyF11 = [122]
fromKey KeyF12 = [123]
{-
fromKey KeyPadDel = [46]
fromKey KeyPadIns = [45]
fromKey KeyPadEnd = [35]
fromKey KeyPadDown = [40]
fromKey KeyPadPgDown = [34]
fromKey KeyPadLeft = [37]
fromKey KeyPadRight = [39]
fromKey KeyPadHome = [36]
fromKey KeyPadUp = [38]
fromKey KeyPadPgUp = [33]
-}
fromKey KeyPadAdd = [107]
fromKey KeyPadSub = [109]
fromKey KeyPadMul = [106]
fromKey KeyPadDiv = [111]
fromKey KeyPadEnter = [13]
fromKey KeyPadDot = [46]
fromKey KeyPad0 = [48]
fromKey KeyPad1 = [49]
fromKey KeyPad2 = [50]
fromKey KeyPad3 = [51]
fromKey KeyPad4 = [52]
fromKey KeyPad5 = [53]
fromKey KeyPad6 = [54]
fromKey KeyPad7 = [55]
fromKey KeyPad8 = [56]
fromKey KeyPad9 = [57]

fromMouseButton :: MouseButton -> RawMouseButton
fromMouseButton MouseLeft = 0
fromMouseButton MouseMiddle = 1
fromMouseButton MouseRight = 2

#else

type RawKey = GLFW.Key
type RawMouseButton = GLFW.MouseButton

fromKey :: Key -> [RawKey]
fromKey KeyA = [GLFW.Key'A]
fromKey KeyB = [GLFW.Key'B]
fromKey KeyC = [GLFW.Key'C]
fromKey KeyD = [GLFW.Key'D]
fromKey KeyE = [GLFW.Key'E]
fromKey KeyF = [GLFW.Key'F]
fromKey KeyG = [GLFW.Key'G]
fromKey KeyH = [GLFW.Key'H]
fromKey KeyI = [GLFW.Key'I]
fromKey KeyJ = [GLFW.Key'J]
fromKey KeyK = [GLFW.Key'K]
fromKey KeyL = [GLFW.Key'L]
fromKey KeyM = [GLFW.Key'M]
fromKey KeyN = [GLFW.Key'N]
fromKey KeyO = [GLFW.Key'O]
fromKey KeyP = [GLFW.Key'P]
fromKey KeyQ = [GLFW.Key'Q]
fromKey KeyR = [GLFW.Key'R]
fromKey KeyS = [GLFW.Key'S]
fromKey KeyT = [GLFW.Key'T]
fromKey KeyU = [GLFW.Key'U]
fromKey KeyV = [GLFW.Key'V]
fromKey KeyW = [GLFW.Key'W]
fromKey KeyX = [GLFW.Key'X]
fromKey KeyY = [GLFW.Key'Y]
fromKey KeyZ = [GLFW.Key'Z]
fromKey Key0 = [GLFW.Key'0]
fromKey Key1 = [GLFW.Key'1]
fromKey Key2 = [GLFW.Key'2]
fromKey Key3 = [GLFW.Key'3]
fromKey Key4 = [GLFW.Key'4]
fromKey Key5 = [GLFW.Key'5]
fromKey Key6 = [GLFW.Key'6]
fromKey Key7 = [GLFW.Key'7]
fromKey Key8 = [GLFW.Key'8]
fromKey Key9 = [GLFW.Key'9]
fromKey KeySpace = [GLFW.Key'Space]
fromKey KeyEnter = [GLFW.Key'Enter]
fromKey KeyTab = [GLFW.Key'Tab]
fromKey KeyEsc = [GLFW.Key'Escape]
fromKey KeyBackspace = [GLFW.Key'Backspace]
fromKey KeyShift = [GLFW.Key'LeftShift, GLFW.Key'RightShift]
fromKey KeyControl = [GLFW.Key'LeftControl, GLFW.Key'RightControl]
fromKey KeyAlt = [GLFW.Key'LeftAlt, GLFW.Key'RightAlt]
fromKey KeyCapsLock = [GLFW.Key'CapsLock]
fromKey KeyNumLock = [GLFW.Key'NumLock]
fromKey KeyArrowLeft = [GLFW.Key'Left]
fromKey KeyArrowUp = [GLFW.Key'Up]
fromKey KeyArrowRight = [GLFW.Key'Right]
fromKey KeyArrowDown = [GLFW.Key'Down]
fromKey KeyIns = [GLFW.Key'Insert]
fromKey KeyDel = [GLFW.Key'Delete]
fromKey KeyHome = [GLFW.Key'Home]
fromKey KeyEnd = [GLFW.Key'End]
fromKey KeyPgUp = [GLFW.Key'PageUp]
fromKey KeyPgDown = [GLFW.Key'PageDown]
fromKey KeyF1 = [GLFW.Key'F1]
fromKey KeyF2 = [GLFW.Key'F2]
fromKey KeyF3 = [GLFW.Key'F3]
fromKey KeyF4 = [GLFW.Key'F4]
fromKey KeyF5 = [GLFW.Key'F5]
fromKey KeyF6 = [GLFW.Key'F6]
fromKey KeyF7 = [GLFW.Key'F7]
fromKey KeyF8 = [GLFW.Key'F8]
fromKey KeyF9 = [GLFW.Key'F9]
fromKey KeyF10 = [GLFW.Key'F10]
fromKey KeyF11 = [GLFW.Key'F11]
fromKey KeyF12 = [GLFW.Key'F12]
fromKey KeyPadAdd = [GLFW.Key'PadAdd]
fromKey KeyPadSub = [GLFW.Key'PadSubtract]
fromKey KeyPadMul = [GLFW.Key'PadMultiply]
fromKey KeyPadDiv = [GLFW.Key'PadDivide]
fromKey KeyPadEnter = [GLFW.Key'PadEnter]
fromKey KeyPadDot = [GLFW.Key'PadDecimal]
fromKey KeyPad0 = [GLFW.Key'Pad0]
fromKey KeyPad1 = [GLFW.Key'Pad1]
fromKey KeyPad2 = [GLFW.Key'Pad2]
fromKey KeyPad3 = [GLFW.Key'Pad3]
fromKey KeyPad4 = [GLFW.Key'Pad4]
fromKey KeyPad5 = [GLFW.Key'Pad5]
fromKey KeyPad6 = [GLFW.Key'Pad6]
fromKey KeyPad7 = [GLFW.Key'Pad7]
fromKey KeyPad8 = [GLFW.Key'Pad8]
fromKey KeyPad9 = [GLFW.Key'Pad9]

fromMouseButton :: MouseButton -> RawMouseButton
fromMouseButton MouseLeft = GLFW.MouseButton'1
fromMouseButton MouseMiddle = GLFW.MouseButton'3
fromMouseButton MouseRight = GLFW.MouseButton'2

#endif
