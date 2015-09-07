module Graphics.Rendering.Ombra.Color where

import Control.Applicative
import Data.Hashable
import Data.Word (Word8)
import Foreign.Ptr (castPtr)
import Foreign.Storable

-- | An RGBA 32-bit color.
data Color = Color !Word8 !Word8 !Word8 !Word8 deriving (Eq, Show)

instance Hashable Color where
        hashWithSalt salt (Color r g b a) = hashWithSalt salt (r, g, b, a)

instance Storable Color where
        sizeOf _ = 4 * sizeOf (undefined :: Word8)
        alignment _ = alignment (undefined :: Word8)
        peek p = Color <$> peekElemOff bp 0 <*> peekElemOff bp 1
                       <*> peekElemOff bp 2 <*> peekElemOff bp 3
                where bp = castPtr p
        poke p (Color r g b a) = do pokeElemOff bp 0 r
                                    pokeElemOff bp 1 g
                                    pokeElemOff bp 2 b
                                    pokeElemOff bp 3 a
                where bp = castPtr p

-- | Create a 'Color' with alpha set to 255.
visible :: Word8 -> Word8 -> Word8 -> Color
visible r g b = Color r g b 255

white :: Color
white = Color 255 255 255 255

black :: Color
black = Color 0 0 0 255

transparent :: Color
transparent = Color 0 0 0 0

red :: Color
red = Color 255 0 0 255

green :: Color
green = Color 0 255 0 255

blue :: Color
blue = Color 0 255 255 255

yellow :: Color
yellow = Color 255 255 0 255
