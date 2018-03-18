{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.Rendering.Ombra.Draw.Set (
        DrawSet,
        singleton,
        singletonShader,
        before,
        clear,
        drawSimple,
        DrawFeedback,
        drawACS,

        -- * DrawMode
        DrawMode,
        mode,
        bufferMode,
        blend,
        noBlend,
        stencil,
        noStencil,
        depthTest,
        depthMask,
        colorMask,
        preVertex,
        extVertex,
        preFragment,
) where

import Data.Ant
import Data.Either (isRight)
import Data.Hashable (hash)
import qualified Data.HashSet as HS
import qualified Data.HashMap.Lazy as HM
import Data.List.NonEmpty (toList)
import qualified Data.Map.Lazy as M
import Data.Maybe (maybe)
import Data.Semigroup
import qualified Data.Set as S
import Graphics.Rendering.Ombra.Backend (GLES)
import Graphics.Rendering.Ombra.Draw.Class
import Graphics.Rendering.Ombra.Draw.Mode
import Graphics.Rendering.Ombra.Draw.State
import Graphics.Rendering.Ombra.Geometry
import Graphics.Rendering.Ombra.OutBuffer
import Graphics.Rendering.Ombra.Shader
import System.Random hiding (next)

newtype DrawMap o = DrawMap (M.Map DrawChange
                                   (Either (DrawMap o)
                                           (S.Set (DrawState o))))

instance GLES => Semigroup (DrawMap o) where
        DrawMap m <> DrawMap m' = DrawMap $
                M.unionWith (\e e' -> case (e, e') of
                                           (Left m, Left m') ->
                                                   Left $ m <> m'
                                           (Right s, Right s') ->
                                                   Right $ S.union s s'
                            )
                            m
                            m'

instance GLES => Monoid (DrawMap o) where
        mempty = DrawMap M.empty
        mappend = (<>)

-- | A container for drawing operations.
data DrawSet o = Set (DrawMap o) | Seq (DrawSet o) (DrawMap o) (DrawSet o)

instance GLES => Semigroup (DrawSet o) where
        Set m <> Set m' = Set (m <> m')
        Set m <> Seq b m' a = Seq b (m <> m') a
        Seq b m a <> Set m' = Seq b (m <> m') a
        Seq b m a <> Seq b' m' a' = Seq (b <> b') (m <> m') (a <> a')

instance GLES => Monoid (DrawSet o) where
        mempty = Set mempty
        mappend = (<>)

-- | Create a set with only one element.
singleton :: ( GLES
             , GeometryVertex g
             , ElementType e
             , ShaderInput g
             , ShaderInput v
             , FragmentShaderOutput o
             )
          => Geometry e g
          -> DrawMode g v v o
          -> DrawSet o
singleton geom mode = let state = modeToState geom mode
                          changes = rootChanges state
                          Left map = foldr (\c -> Left . DrawMap . M.singleton c)
                                           (Right $ S.singleton state)
                                           changes
                      in Set map

-- | Combination of 'singleton', 'preVertex' and 'preFragment'.
singletonShader :: ( GLES
                   , GeometryVertex g
                   , ElementType e
                   , ShaderInput g
                   , ShaderInput v
                   , FragmentShaderOutput o
                   )
                => Geometry e g
                -> VertexShader g vi
                -> FragmentShader v fi
                -> DrawMode vi v fi o
                -> DrawSet o
singletonShader geom vs fs = singleton geom . preVertex vs . preFragment fs

-- | This is a special set element that clears the buffers before drawing
-- anything to them. @x <> (clear ...)@ behaves like @before (clear ...) x@
-- if the two actions draw to the same buffers, but it's not true that
-- @before x (clear ...)@ is equivalent to @before (clear ...) x@, because the
-- order determined by 'before' is prioritized.
clear :: GLES
      => Bool                   -- ^ Clear the color buffer.
      -> Bool                   -- ^ Clear the depth buffer.
      -> Bool                   -- ^ Clear the stencil buffer.
      -> Maybe (BufferPair o')  -- ^ Target.
      -> DrawSet o
clear color depth stencil mBuf =
        let targ = case mBuf of
                        Just buf -> BufferTarget buf
                        Nothing -> DefaultTarget
        in Set . DrawMap
               . M.singleton (rootTarget targ)
               . Left
               . DrawMap
               . M.singleton (ClearBuffers (color, depth, stencil))
               . Left
               $ DrawMap M.empty

infixr 5 `before`
-- | Merge the two sets so that the second one is drawn after the first. Use
-- this only if it's really necessary to draw the routes in a certain order
-- (e.g. when depth test is disabled), in any other case use ('<>').
before :: GLES => DrawSet o -> DrawSet o -> DrawSet o
before s s' = Seq s mempty s'

-- | Draw a 'DrawSet'.
drawSimple :: (GLES, FragmentShaderOutput o, MonadDraw o m)
           => DrawSet o
           -> m ()
drawSimple (Set s) = drawMap [] s
        where snd3 (x, y, z) = y
              drawMap chgs (DrawMap map) = snd3 $
                M.foldlWithKey (\(first, act, clearChgs) chg e ->
                                let chgs' | first = (True, chg) :
                                                    (clearChgs ++ chgs)
                                          | otherwise = [(False, chg)]
                                in case chg of
                                        ClearBuffers _ -> ( first
                                                          , act
                                                          , (False, chg) :
                                                            clearChgs
                                                          )
                                        _ -> ( False
                                             , act >>
                                               (case e of
                                                     Left m -> drawMap chgs' m
                                                     Right s ->
                                                        fst $ drawSet chgs' s
                                               )
                                             , []
                                             )
                               )
                               (True, return (), [])
                               map
drawSimple s = drawAll $ inner True s
        where inner _ (Seq b m a) = let (bb, bd, ba) = inner False b
                                        (ab, ad, aa) = inner True a
                                        (Left b', Left a') =
                                                mergeAll (Left m)
                                                         (Left ba, Left ab)
                                        d' = do bd
                                                drawSimple (Set b')
                                                drawSimple (Set a')
                                                ad
                                    in (bb, d', aa)
              inner False (Set m) = (DrawMap M.empty, return (), m)
              inner True (Set m) = (m, return (), DrawMap M.empty)

              drawAll (DrawMap b, d, DrawMap a) | M.null b && M.null a = d
              drawAll (b, d, a) = drawSimple (Set b) >> d >> drawSimple (Set a)

              mergeAll (Left (DrawMap mm)) (Left b, Left a) =
                      let (b', a') = M.foldrWithKey mergeBest (b, a) mm
                      in (Left b', Left a')
              mergeAll (Right m) (Right b, a) = (Right (b <> m), a)
              mergeAll (Right m) (b, Right a) = (b, Right (a <> m))
              mergeAll (Left m) (Left b, a) = (Left (m <> b), a)
              mergeAll (Left m) (b, Left a) = (b, Left (m <> a))
              mergeAll _ _ = error "drawSimple: invalid set"

              mergeBest chg e (b@(DrawMap bm), a@(DrawMap am)) =
                        case (M.lookup chg bm, M.lookup chg am) of
                                (Just be, Just ae) ->
                                        let (be', ae') = mergeAll e (be, ae)
                                        in ( DrawMap $ M.insert chg be' bm
                                           , DrawMap $ M.insert chg ae' am
                                           )
                                (Nothing, Just _) -> ( b
                                                     , a <>
                                                       DrawMap (M.singleton chg
                                                                            e)
                                                     )
                                _ -> ( b <> DrawMap (M.singleton chg e)
                                     , a
                                     )

drawSet :: (GLES, FragmentShaderOutput o, MonadDraw o m)
        => [(Bool, DrawChange)]
        -> S.Set (DrawState o)
        -> (m (), Maybe (DrawState o))
drawSet chgs set =
        let iter (act, lastState) s' =
                ( do act
                     s <- currentState
                     let chg = changedUniforms s s'
                         chgs' | Nothing <- lastState = (True, chg) : chgs
                               | otherwise = [(False, chg)]
                         chgs'' = chgs' >>= \(init, chg) ->
                                  diffState init s s' chg
                     switchState True s' chgs''
                , Just s'
                )
            (act, lastState) = foldl iter (return (), Nothing) set
        in (act, lastState)

newtype DrawFeedback o =
        DrawFeedback { unDrawFeedback :: Feedback (Either Int (DrawState o))
                                                  DrawMoveType
                                                  Float
                     } 

type DrawMove o m = Move (Either Int (DrawState o))
                         DrawMoveType
                         Float
                         (ACSState o m)

data DrawMoveType = Stay | Change DrawChange | Smaller | Larger | Before Int
        deriving (Eq, Ord)

data ACSState o m = ACSState { rng :: StdGen
                             , remainingSet :: DrawSet o
                             , currentTrail :: m ()
                             , currentLength :: Float
                             , bestTrail :: m ()
                             , bestLength :: Float
                             , minLength :: Float
                             , iterations :: Int
                             }

-- | Draw a 'DrawSet'. Compared to 'drawSimple', this uses a more CPU intensive
-- nondeterministic algorithm to determine the best way to draw the set.
drawACS :: (GLES, FragmentShaderOutput o, MonadDraw o m)
        => Maybe (DrawFeedback o)       -- ^ Feedback from the previous drawACS
                                        -- call (Nothing if this is the first
                                        -- time).
        -> StdGen
        -> DrawSet o
        -> (m (), (DrawFeedback o, StdGen))
drawACS fb stdGen set =
        let maxIterations = 1
            initAcc = ACSState { rng = stdGen
                               , remainingSet = set
                               , currentTrail = return ()
                               , currentLength = 1 / 0
                               , bestTrail = return ()
                               , bestLength = 1 / 0
                               , minLength = 0
                               , iterations = 0
                               }
            genRandom acc = let (n, rng') = randomR (0, 1) (rng acc)
                            in (n, acc { rng = rng' })
            genStates acc = let (n1, stdGen) = random $ rng acc
                                (n2, stdGen') = random stdGen
                                (n3, stdGen'') = random stdGen'
                            in (map Left [n1, n2, n3], acc { rng = stdGen'' })
            nextIter acc | iterations acc >= maxIterations = Nothing
                         | otherwise = Just acc { iterations = iterations acc + 1
                                                }
            initAnt _ acc = let better = currentLength acc < bestLength acc
                            in acc { remainingSet = set
                                   , currentTrail = return ()
                                   , currentLength = 0
                                   , bestTrail = if better
                                                 then currentTrail acc
                                                 else bestTrail acc
                                   , bestLength = if better
                                                  then currentLength acc
                                                  else bestLength acc
                                   }
            genMoves state acc = moves (fmap (toList . rootChanges) state)
                                       (maybe (Set $ DrawMap M.empty) id)
                                       0
                                       (remainingSet acc)
            score acc = minLength acc / currentLength acc
            (fb', acc') = simulate (Parameters 6 0.4 1 0.0001)
                                   genRandom
                                   genStates
                                   initAnt
                                   genMoves
                                   score
                                   nextIter
                                   (fmap unDrawFeedback fb)
                                   initAcc
        in (bestTrail acc', (DrawFeedback fb', rng acc'))

moves :: (MonadDraw o m, FragmentShaderOutput o)
      => Either Int [DrawChange]                -- ^ Root changes, or a random
                                                -- number.
      -> (Maybe (DrawSet o) -> DrawSet o)       -- ^ Build a new set with the
                                                -- visited states removed.
      -> Int                                    -- ^ Number of times the set on
                                                -- the left was chosen, instead
                                                -- of the unordered (middle)
                                                -- one.
      -> DrawSet o                              -- ^ Current set.
      -> [DrawMove o m]
moves _ _ _ (Set (DrawMap m)) | M.null m = []
moves chgs mkSet _ (Set m) = mapMoves chgs [] (mkSet . fmap Set . fromMLeftMap) m
moves chgs mkSet n (Seq b m@(DrawMap unorderedMap) a) =
        let beforeMoves = moves chgs
                                (\mb' ->
                                   case mb' of
                                        Just b' -> mkSet . Just $ Seq b' m a
                                        Nothing ->
                                            if M.null unorderedMap
                                            then mkSet . Just $ a
                                            else mkSet . Just $ Seq (Set m)
                                                                    mempty
                                                                    a
                                )
                                (n + 1)
                                b
            middleMkSet = mkSet . fmap (\m' -> Seq b m' a) . fromMLeftMap
            middleMoves = mapMoves chgs [] middleMkSet m
        in if null beforeMoves
           then middleMoves
           else   Move { moveType = Just $ Before n
                       , seqFeedback = True
                       , desiderability = 1
                       , nextAccumulator = id
                       , next = Left beforeMoves
                       }
                : middleMoves

mLeftMap :: DrawMap o -> Maybe (Either (DrawMap o) a)
mLeftMap (DrawMap m) | M.null m = Nothing
                     | otherwise = Just . Left $ DrawMap m

fromMLeftMap :: Maybe (Either (DrawMap o) a) -> Maybe (DrawMap o)
fromMLeftMap (Just (Left dm@(DrawMap m))) | not (M.null m) = Just dm
fromMLeftMap _ = Nothing

mapMoves :: (MonadDraw o m, FragmentShaderOutput o)
         => Either Int [DrawChange]             -- ^ Remaining root changes of
                                                -- the current state, or a
                                                -- random number.
         -> [DrawChange]                        -- ^ Changes from the previous
                                                -- moves.
         -> (   Maybe (Either (DrawMap o) (S.Set (DrawState o)))
             -> DrawSet o
            )                                   -- ^ Build a new set with the
                                                -- visited states removed.
         -> DrawMap o                           -- ^ Original map.
         -> [DrawMove o m]
mapMoves _ _ _ (DrawMap m) | M.null m = []
mapMoves chgs schgs mkSet (DrawMap m)
        | Just (chg'@(ClearBuffers _), _) <- M.lookupMin m =
        mapMoves chgs (chg' : schgs) mkSet (DrawMap $ M.delete chg' m)
mapMoves (Left rand) schgs mkSet (DrawMap m) =
        let (chg, e) = M.elemAt (rand `mod` M.size m) m
            mkSet' s = mkSet . mLeftMap . DrawMap $ M.update (const s) chg m
        in maybe [] (: []) $ mkMove (Just chg) (Left $ hash rand) schgs mkSet' e
mapMoves (Right (chg : chgs)) schgs mkSet (DrawMap m) | M.size m > 6 = 
        let (ml, Just e, mg) = M.splitLookup chg m
            split5 map | M.null map = []
                       | otherwise = let (m5, mr) = M.splitAt 5 map
                                     in if M.null mr
                                        then [m5]
                                        else m5 : split5 mr
            cost = changeCost chg
            splitMove t m = Move { moveType = Just t
                                 , seqFeedback = True
                                 , desiderability = 1 / cost
                                 , nextAccumulator = id
                                 , next = Left $ mapMoves (Right $ chg : chgs)
                                                          schgs
                                                          mkSet
                                                          (DrawMap m)
                                 }
            mls = map (splitMove Smaller) $ split5 ml
            mgs = map (splitMove Larger) $ split5 mg
            mkSet' s = mkSet . mLeftMap . DrawMap $ M.update (const s) chg m
            stayMove = mkMove Nothing (Right chgs) schgs mkSet' e
        in maybe (mls ++ mgs) (: (mls ++ mgs)) stayMove
mapMoves (Right (chg : chgs)) schgs mkSet (DrawMap m) =
        M.foldrWithKey (\chg' e ->
                           let dchg = diffChange chg chg'
                               mkSet' s = mkSet . mLeftMap . DrawMap $
                                                M.update (const s) chg' m
                               move = mkMove dchg (Right chgs) schgs mkSet' e
                           in maybe id (:) move
                       )
                       []
                       m

mkMove :: (MonadDraw o m, FragmentShaderOutput o)
       => Maybe DrawChange              -- ^ Change required for this move.
       -> Either Int [DrawChange]       -- ^ Remaining root changes.
       -> [DrawChange]                  -- ^ Changes from the previous moves.
       -> (Maybe (Either (DrawMap o) (S.Set (DrawState o))) -> DrawSet o)
       -> Either (DrawMap o) (S.Set (DrawState o))
       -> Maybe (DrawMove o m)
mkMove chg chgs schgs mkSet e =
        let cost = maybe 1 changeCost chg
            dchgs = maybe schgs (: schgs) chg
            (nextAccumulator, next) =
                    case e of
                         Left m -> (incCost, Left $ mapMoves chgs dchgs mkSet m)
                         Right s -> let (act, last) =
                                                drawSet (map ((,) True) dchgs)
                                                        s
                                    in ( incCost . newState act s
                                       , Right $ maybe (Left 0) Right last
                                       )
            incCost a = a { currentLength = currentLength a + cost
                          , minLength = if iterations a == 0
                                        then minLength a + cost
                                        else minLength a
                          }
            newState act s a = a { currentTrail = currentTrail a >> act
                                 , remainingSet = mkSet Nothing
                                 }
        in case next of
                Left [] -> Nothing
                _ -> Just $ Move { moveType = if isRight chgs
                                              then Just $ maybe Stay Change chg
                                              else Nothing
                                 , seqFeedback = False
                                 , desiderability = 1 / cost
                                 , nextAccumulator = nextAccumulator
                                 , next = next
                                 }

changeCost :: DrawChange -> Float
changeCost (ChangeTarget _) = 500
changeCost (ClearBuffers _) = 1
changeCost (ChangeShaders _) = 100
changeCost (ChangeTextures added _) = 1 + fromIntegral (HS.size added) * 10
changeCost (ChangeBlendEnabled _) = 10
changeCost (ChangeBlendConstantColor _) = 10
changeCost (ChangeBlendEquation _) = 10
changeCost (ChangeBlendFunction _) = 10
changeCost (ChangeStencilEnabled _) = 10
changeCost (ChangeStencilFunction _) = 10
changeCost (ChangeStencilOperation _) = 10
changeCost (ChangeCullEnabled _) = 10
changeCost (ChangeCullFace _) = 10
changeCost (ChangeDepthTest _) = 10
changeCost (ChangeDepthMask _) = 10
changeCost (ChangeColorMask _) = 10
changeCost (ChangeGeometry _) = 5
changeCost (ChangeUniforms changed) = 1 + fromIntegral (HM.size changed)
