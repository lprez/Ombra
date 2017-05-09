{-# LANGUAGE CPP #-}

#ifdef __GHCJS__
{-# LANGUAGE JavaScriptFFI, InterruptibleFFI #-}
#endif

module Utils.TextureLoader (
        loadTexture
) where

import Graphics.Rendering.Ombra
import Data.Hashable

#ifdef __GHCJS__

import Data.String
import GHCJS.Types
import JavaScript.TypedArray
import JavaScript.TypedArray.Internal
import JavaScript.TypedArray.DataView

foreign import javascript unsafe "$1.width" imgWidth :: JSVal -> IO Int
foreign import javascript unsafe "$1.height" imgHeight :: JSVal -> IO Int

foreign import javascript unsafe "$1.data" imgData :: JSVal -> IO Uint8Array

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

loadTexture :: GLES => FilePath -> IO Texture
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
import qualified Data.Vector.Storable as V

loadTexture :: GLES => FilePath -> IO Texture
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
