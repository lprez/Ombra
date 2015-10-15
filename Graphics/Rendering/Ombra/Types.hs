{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators, FlexibleContexts,
             ExistentialQuantification, GeneralizedNewtypeDeriving #-}

module Graphics.Rendering.Ombra.Types (
        Draw(..),
        DrawState(..),
        UniformLocation(..),
        Texture(..),
        TextureImage(..),
        LoadedTexture(..),
        Geometry(..),
        Group(..),
        Object(..),
        Global(..),
        Layer(..),
        RenderLayer(..),
        LayerType(..)
) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Data.Hashable
import Data.Vect.Float hiding (Vector)
import Data.Vector (Vector)
import Data.Typeable
import Data.Word (Word8)
import Graphics.Rendering.Ombra.Geometry
import Graphics.Rendering.Ombra.Color
import Graphics.Rendering.Ombra.Internal.GL hiding (Program, Texture, UniformLocation)
import qualified Graphics.Rendering.Ombra.Internal.GL as GL
import Graphics.Rendering.Ombra.Internal.TList
import Graphics.Rendering.Ombra.Internal.Resource
import Graphics.Rendering.Ombra.Shader.CPU
import Graphics.Rendering.Ombra.Shader.Program
import Graphics.Rendering.Ombra.Shader.ShaderVar

newtype UniformLocation = UniformLocation GL.UniformLocation

-- | The state of the 'Draw' monad.
data DrawState = DrawState {
        currentProgram :: Maybe (Program '[] '[]),
        loadedProgram :: Maybe LoadedProgram,
        programs :: ResMap (Program '[] '[]) LoadedProgram,
        uniforms :: ResMap (LoadedProgram, String) UniformLocation,
        gpuBuffers :: ResMap (Geometry '[]) GPUBufferGeometry,
        gpuVAOs :: ResMap (Geometry '[]) GPUVAOGeometry,
        textureImages :: ResMap TextureImage LoadedTexture,
        activeTextures :: Vector (Maybe Texture),
        viewportSize :: (Int, Int)
}

-- | A state monad on top of 'GL'.
newtype Draw a = Draw { unDraw :: StateT DrawState GL a }
        deriving (Functor, Applicative, Monad, MonadIO)

instance EmbedIO Draw where
        embedIO f (Draw a) = Draw get >>= Draw . lift . embedIO f . evalStateT a

-- | A texture.
data Texture = TextureImage TextureImage
             | TextureLoaded LoadedTexture
             deriving Eq
             
data TextureImage = TexturePixels [Color] GLSize GLSize Int

data LoadedTexture = LoadedTexture GLSize GLSize GL.Texture

-- | A group of 'Object's.
data Group (gs :: [*]) (is :: [*]) where
        Empty :: Group gs is
        Object :: Object gs is -> Group gs is
        Global :: Global g -> Group gs is -> Group (g ': gs) is
        Append :: Group gs is -> Group gs' is' -> Group gs'' is''

-- | A geometry associated with some uniforms.
data Object (gs :: [*]) (is :: [*]) where
        (:~>) :: Global g -> Object gs is -> Object (g ': gs) is
        Mesh :: Geometry is -> Object '[] is
        NoMesh :: Object '[] '[]

infixr 2 :~>

-- | The value of a GPU uniform.
data Global g where
        (:=) :: (ShaderVar g, Uniform 'S g)
             => (a -> g) -> Draw (CPU 'S g) -> Global g

infix 3 :=

-- | A 'Group' associated with a program.
data Layer = forall oi pi og pg. (Subset pi oi, Subset pg og)
                              => Layer (Program pg pi) (Group og oi)
           | SubLayer (RenderLayer [Layer])
           | MultiLayer [Layer]

-- | Represents a 'Layer' drawn on a 'Texture'.
data RenderLayer a = RenderLayer [LayerType] Int Int
                                 Int Int Int Int
                                 Bool Bool Layer
                                 ([Texture] -> Maybe [Color] ->
                                  Maybe [Word8] -> a)

data LayerType = ColorLayer | DepthLayer deriving Eq

instance Hashable TextureImage where
        hashWithSalt salt tex = hashWithSalt salt $ textureHash tex

instance Eq TextureImage where
        (TexturePixels _ _ _ h) == (TexturePixels _ _ _ h') = h == h'

instance GLES => Eq LoadedTexture where
        LoadedTexture _ _ t == LoadedTexture _ _ t' = t == t'

textureHash :: TextureImage -> Int
textureHash (TexturePixels _ _ _ h) = h
