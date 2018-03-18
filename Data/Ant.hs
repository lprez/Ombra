module Data.Ant (
        Parameters(..),
        Feedback(..),
        Move(..),
        simulate,
        tsp
) where


import Control.Monad (join)
import qualified Data.Map.Lazy as M
import qualified Data.Map.Merge.Lazy as M
import Data.Maybe
import qualified Data.Set as S
import System.Random hiding (next)

newtype MoveFeedback m c = MoveFeedback (M.Map m (c, Maybe (MoveFeedback m c)))
        deriving (Show)

newtype Feedback s m c = Feedback (M.Map s (MoveFeedback m c))
        deriving (Show)

-- | Represents a micro-move that doesn't necessarily bring the simulation to a
-- new state.
data Move s m c a = Move {
                           -- | Anything that can identify the move. If Nothing,
                           -- the Feedback won't be affected by neither this
                           -- move nor all its successors (even if their
                           -- moveType is not Nothing).
                           moveType :: Maybe m

                         , -- | True if the move type is not unique among the
                           -- ones that follow from the same state.
                           seqFeedback :: Bool

                         , -- | Desiderability (e.g. inverse distance) of this
                           -- move.
                           desiderability :: c

                         , -- | How the accumulator should change when this move
                           -- is chosen.
                           nextAccumulator :: a -> a

                         , -- | All the possible moves that follow this one, or
                           -- the new state if there are no more. If Left, the
                           -- list must not be empty.
                           next :: Either [Move s m c a] s
                         }

data Parameters c =
        Parameters { -- | How much the desiderability of the moves influences
                     -- the choice of the moves.
                     desiderabilityExp :: c

                   , -- | How fast the previous iterations are forgotten (0-1).
                     pheromoneEvaporation :: c

                   , -- ^ Estimated optimal trail score.
                     pheromoneMaximum :: c

                   , -- ^ Minimum score of a trail.
                     pheromoneMinimum :: c
                   }
        deriving (Eq, Show)

exampleParameters :: Fractional c => Parameters c
exampleParameters = Parameters 6 0.4 1 0.0002

tsp :: (Floating c, Ord c, Random c, Eq s, Ord s)
    => Parameters c
    -> Int                              -- ^ Number of ants for each iteration.
    -> S.Set s                          -- ^ Set of all possible states.
    -> (s -> s -> c)                    -- ^ Distance between two states.
    -> (Int -> [(c, [s])] -> Bool)      -- ^ True if the simulation should
                                        -- continue, based on the number
                                        -- of iterations and the trails
                                        -- produced so far.
    -> Maybe (Feedback s s c)           -- ^ Feedback from the previous
                                        -- simulation.
    -> StdGen
    -> (Feedback s s c, StdGen, [(c, [s])])
tsp params nAnts allStates distance cont mFeedback stdGen =
        let (feedback, (stdGen', _, _, trails)) =
                simulate params
                         (\(gen, n, ss, ts) -> let (r, gen') = randomR (0, 1) gen
                                               in (r, (gen', n, ss, ts))
                         )
                         (\acc -> (!! nAnts) . flip iterate ([], acc) $
                                \(is, (gen, n, ss, trails)) ->
                                        let maxIdx = S.size allStates - 1
                                            (idx, gen') = randomR (0, maxIdx) gen
                                            initState = S.elemAt idx allStates
                                        in (initState : is, (gen', n, ss, trails))
                         )
                         (\s (gen, n, ss, ts) ->
                                 (gen, n, S.delete s allStates, ((0, [s]) : ts))
                         )
                         (\s (_, _, ss, ts) -> flip map (S.toList ss) $ \s' ->
                                let dist = distance s s'
                                    af (gen, n, ss, ((c, t) : ts)) =
                                            ( gen
                                            , n
                                            , S.delete s' ss
                                            , (c + dist, (s' : t)) : ts
                                            )
                                in Move (Just s') False (1 / dist) af (Right s')
                         )
                         (\(_, _, _, (d, t) : _) -> 1 / (d + distance (head t)
                                                                      (last t)))
                         (\(gen, n, ss, ts) -> if cont n ts
                                               then Just (gen, n + 1, ss, ts)
                                               else Nothing
                         )
                         mFeedback
                         (stdGen, 1, allStates, [])
        in (feedback, stdGen', map (\(c, t) -> (c, reverse t)) trails)

simulate :: (Floating c, Ord c, Eq m, Ord m, Eq s, Ord s)
         => Parameters c
         -> (a -> (c, a))               -- ^ Generate a random number in [0,1].
         -> (a -> ([s], a))             -- ^ Generate a list of random
                                        -- initial states, one for each ant.
         -> (s -> a -> a)               -- ^ Initialize the accumulator for
                                        -- each ant. The first argument is the
                                        -- first state.
         -> (s -> a -> [Move s m c a])  -- ^ List of possible moves from a
                                        -- certain state.
         -> (a -> c)                    -- ^ Score of the trail produced at a
                                        -- certain point of the simulation.
         -> (a -> Maybe a)              -- ^ Function to call after every
                                        -- iteration. If 'Just', the next
                                        -- iteration will use the new
                                        -- accumulator; if 'Nothing', the
                                        -- simulation stops.
         -> Maybe (Feedback s m c)      -- ^ Feedback from the previous simulation.
         -> a                           -- ^ Initial accumulator value.
         -> (Feedback s m c, a)
simulate params random genStates reset movesFrom scoreOf nextAcc mfb initAcc =
        iter initAcc (fromMaybe (Feedback M.empty) mfb)
        where iter acc oldFb =
                let fb = evaporate params oldFb
                    (initStates, acc') = genStates acc
                    (newFb, acc'', best, _) = foldr (iterAnt best fb)
                                                    (fb, acc', 0, False)
                                                    initStates
                in case nextAcc acc'' of
                        Just next -> iter next newFb
                        Nothing -> (newFb, acc'')

              iterAnt best oldFb initState (fb, acc, iterBest, foundBest) =
                      let (newFb, acc') = trail params
                                                random
                                                movesFrom
                                                (reset initState acc)
                                                oldFb
                                                (Feedback M.empty)
                                                score
                                                initState
                          score = scoreOf acc'
                          min =   (pheromoneMinimum params)
                                * (1 - pheromoneEvaporation params)
                      in ( if score >= best && not foundBest
                           then feedbackUnion min fb newFb
                           else fb
                         , acc'
                         , if score > iterBest
                           then score
                           else iterBest
                         , foundBest || score >= best
                         )


-- | Move the ant until there are no moves left.
trail :: (Floating c, Ord c, Eq m, Ord m, Eq s, Ord s)
      => Parameters c
      -> (a -> (c, a))
      -> (s -> a -> [Move s m c a])
      -> a
      -> Feedback s m c
      -> Feedback s m c
      -> c
      -> s
      -> (Feedback s m c, a)
trail params random movesFrom acc (Feedback fb) (Feedback newFb) score state =
        let moves = movesFrom state acc
            moveFb = fromMaybe (MoveFeedback M.empty) (fb M.!? state)
            (mMoveFb', acc', state') =
                step params random acc moveFb score (Left moves)
            newFb' = maybe newFb (\mfb -> M.insert state mfb newFb) mMoveFb'
        in case moves of
                [] -> (Feedback newFb, acc)
                _ -> trail params
                           random
                           movesFrom
                           acc'
                           (Feedback fb)
                           (Feedback newFb')
                           score
                           state'

-- | Move the ant to the next state.
step :: (Floating c, Ord c, Eq m, Ord m, Eq s, Ord s)
     => Parameters c
     -> (a -> (c, a))
     -> a
     -> MoveFeedback m c
     -> c
     -> Either [Move s m c a] s
     -> (Maybe (MoveFeedback m c), a, s)
step params random acc (MoveFeedback fb) score (Left moves) =
        let (rand1, acc1) = random acc
            (rand2, acc2) = random acc1
            move = pickMove params rand1 rand2 (MoveFeedback fb) moves
            fb' = fromMaybe (MoveFeedback M.empty) . join . fmap snd $
                    moveType move >>= (fb M.!?)
            (newFb, acc3, state') = step params
                                         random
                                         (nextAccumulator move acc2)
                                         fb'
                                         score
                                         (next move)
            newFb' | Just (MoveFeedback mfb) <- newFb, not (seqFeedback move) =
                           fmap (\t -> M.insert t (score, Nothing) mfb)
                                (moveType move)
                   | otherwise = fmap (\t -> M.singleton t (score, newFb))
                                      (moveType move)
        in (fmap MoveFeedback newFb', acc3, state')
step _ _ acc _ _ (Right state') = (Nothing, acc, state')

evaporate :: (Fractional c, Ord c, Eq s, Ord s, Eq m, Ord m)
          => Parameters c
          -> Feedback s m c
          -> Feedback s m c
evaporate params (Feedback fb) = Feedback $
        M.mapMaybe (\(MoveFeedback fb) ->
                        let fb' = M.mapMaybe evaporateMove fb
                        in if M.null fb'
                           then Nothing
                           else Just $ MoveFeedback fb'
                   )
                   fb
        where persistence = 1 - pheromoneEvaporation params
              minimum = pheromoneMinimum params
              maximum = pheromoneMaximum params / pheromoneEvaporation params
              evaporateMove (p, mfb) =
                      let p' = min maximum $ persistence * p
                      in if p' <= minimum
                         then Nothing
                         else case mfb of
                                   Just (MoveFeedback fb) ->
                                           let fb' = M.mapMaybe evaporateMove fb
                                               mfb' | M.null fb' = Nothing
                                                    | otherwise = Just fb'
                                           in Just (p', fmap MoveFeedback mfb')
                                   Nothing -> Just (p', Nothing)
 
pickMove :: (Floating c, Ord c, Eq m, Ord m)
         => Parameters c
         -> c
         -> c
         -> MoveFeedback m c
         -> [Move s m c a]
         -> Move s m c a
pickMove params rand1 rand2 (MoveFeedback feedback) moves = chosenMove
        where fac move =   (desiderability move ** desiderabilityExp params)
                         * (fromMaybe (pheromoneMinimum params) . fmap fst $
                                        moveType move >>= (feedback M.!?))
              (sum, moves', bestFac, best) =
                      foldr (\move (pos, ms, bestFac, best) ->
                                let f = fac move
                                    pos' = pos + f
                                    ms' = (sum - pos, move) : ms
                                    isBest = f > bestFac
                                    bestFac' | isBest = f
                                             | otherwise = bestFac
                                    best' | isBest = move
                                          | otherwise = best
                                in (pos', ms', bestFac', best')
                            )
                            (0, [], 0, error "ACS error: dead end")
                            moves
              min = sum * rand2
              bestRandom = snd . head $ dropWhile ((< min) . fst) moves'
              chosenMove | rand1 < 0.8 = best
                         | otherwise = bestRandom

feedbackUnion :: (Num c, Eq s, Ord s, Eq m, Ord m)
              => c
              -> Feedback s m c
              -> Feedback s m c
              -> Feedback s m c
feedbackUnion min (Feedback fb) (Feedback fb') = Feedback $
        M.merge (M.mapMissing . const . flip moveFeedbackUnion $ emptyMFb)
                (M.mapMissing . const . moveFeedbackUnion $ emptyMFb)
                (M.zipWithMatched . const $ moveFeedbackUnion)
                fb
                fb'
        where moveFeedbackUnion (MoveFeedback fb) (MoveFeedback fb') =
                MoveFeedback $ M.merge (M.mapMissing . const . flip sumUnion $ def0)
                                       (M.mapMissing . const . sumUnion $ defm)
                                       (M.zipWithMatched . const $ sumUnion)
                                       fb
                                       fb'
              def0 = (0, Just emptyMFb)
              defm = (min, Just emptyMFb)
              sumUnion (p, Nothing) (p', Nothing) = (p + p', Nothing)
              sumUnion (p, fb) (p', fb') =
                      ( p + p'
                      , Just $ moveFeedbackUnion (fromMaybe emptyMFb fb)
                                                 (fromMaybe emptyMFb fb')
                      )
              emptyMFb = MoveFeedback M.empty
