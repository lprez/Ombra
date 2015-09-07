module Graphics.Rendering.Ombra.Internal.STVectorLen where

import Control.Applicative
import Control.Monad (when)
import Control.Monad.ST
import Data.STRef
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M

type STVectorLen s a = (STRef s (M.STVector s a), STRef s Int)

new :: V.Storable a => ST s (STVectorLen s a)
new = (,) <$> (M.new 256 >>= newSTRef) <*> newSTRef 0

(!) :: V.Storable a => STVectorLen s a -> Int -> ST s a
(!) (vRef, _) i = readSTRef vRef >>= flip M.read i

cons :: V.Storable a => a -> STVectorLen s a -> ST s ()
cons x (vRef, lenRef) = do len <- readSTRef lenRef
                           v <- readSTRef vRef
                           let maxLen = M.length v
                           when (len >= maxLen) $ 
                                   M.grow v (maxLen * 2) >>= writeSTRef vRef
                           v' <- readSTRef vRef
                           M.write v' len x
                           writeSTRef lenRef $ len + 1

freeze :: V.Storable a => STVectorLen s a -> ST s (V.Vector a)
freeze (vRef, lenRef) = do v <- readSTRef vRef >>= V.freeze
                           len <- readSTRef lenRef
                           return $ V.slice 0 len v
