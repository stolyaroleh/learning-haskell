{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.StateT where

import Course.Core
import Course.Id
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad
import Course.State
import qualified Data.Set as S
import qualified Prelude as P

class MonadTrans t where
  lift :: Monad m => m a -> t m a

-- $setup
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- | A `StateT` is a function from a state value `s` to a functor f of (a produced value `a`, and a resulting state `s`).
newtype StateT s f a =
  StateT {
    runStateT ::
      s
      -> f (a, s)
  }

-- | Implement the `Functor` instance for @StateT s f@ given a @Functor f@.
--
-- >>> runStateT ((+1) <$> (pure 2) :: StateT Int List Int) 0
-- [(3,0)]
instance Functor f => Functor (StateT s f) where
  (<$>) ::
    (a -> b)
    -> StateT s f a
    -> StateT s f b
  f <$> StateT a =
    StateT $ (mapOnFirst f <$>) . a

-- | Implement the `Applicative` instance for @StateT s f@ given a @Monad f@.
--
-- >>> runStateT (pure 2) 0
-- (2,0)
--
-- >>> runStateT ((pure 2) :: StateT Int List Int) 0
-- [(2,0)]
--
-- >>> runStateT (pure (+2) <*> ((pure 2) :: StateT Int List Int)) 0
-- [(4,0)]
--
-- >>> import qualified Prelude as P
-- >>> runStateT (StateT (\s -> Full ((+2), s P.++ [1])) <*> (StateT (\s -> Full (2, s P.++ [2])))) [0]
-- Full (4,[0,1,2])
--
-- >>> runStateT (StateT (\s -> ((+2), s P.++ [1]) :. ((+3), s P.++ [1]) :. Nil) <*> (StateT (\s -> (2, s P.++ [2]) :. Nil))) [0]
-- [(4,[0,1,2]),(5,[0,1,2])]
instance Monad f => Applicative (StateT s f) where
  pure ::
    a
    -> StateT s f a
  pure a =
    StateT $ \s -> pure (a, s)
  (<*>) ::
    StateT s f (a -> b)
    -> StateT s f a
    -> StateT s f b
  StateT f <*> StateT a =
    -- f :: s -> f ((a -> b), s)
    StateT $ \s -> let f' = f s -- :: f (a -> b, s)
                   in f' >>= \(f'', s') -> mapOnFirst f'' <$> a s' -- :: f (a, s)
    -- StateT ((\(f', s) -> first f' <$> a s) <=< f)
    -- Arcane magic explained:
    -- <=< :: Monad f => (b -> f c) -> (a -> f b) -> a -> f c
    -- f :: s -> f (a -> b, s)
    -- g :: (a -> b, s) -> s -> f (b, s)
    -- g <=< f :: s -> f (b, s)
    -- StateT (g <=< f) :: StateT s f b

-- | Implement the `Monad` instance for @StateT s f@ given a @Monad f@.
-- Make sure the state value is passed through in `bind`.
--
-- >>> runStateT ((const $ putT 2) =<< putT 1) 0
-- ((),2)
--
-- >>> let modify f = StateT (\s -> pure ((), f s)) in runStateT (modify (+1) >>= \() -> modify (*2)) 7
-- ((),16)
instance Monad f => Monad (StateT s f) where
  (=<<) ::
    (a -> StateT s f b)
    -> StateT s f a
    -> StateT s f b
  f =<< StateT sa = StateT $ \s ->
    let inner = sa s -- :: f (a, s)
    in inner >>= \(a, s') -> runStateT (f a) s'

instance MonadTrans (StateT s) where
  -- lift :: Monad m => m a -> StateT s m a
  lift m = StateT $ \s -> do
    a <- m
    return (a, s)

-- | A `State'` is `StateT` specialised to the `Id` functor.
type State' s a =
  StateT s Id a

-- | Provide a constructor for `State'` values
--
-- >>> runStateT (state' $ runState $ put 1) 0
-- Id ((),1)
state' ::
  (s -> (a, s))
  -> State' s a
state' f =
  StateT $ Id . f

-- | Provide an unwrapper for `State'` values.
--
-- >>> runState' (state' $ runState $ put 1) 0
-- ((),1)
runState' ::
  State' s a
  -> s
  -> (a, s)
runState' s = runId . runStateT s

-- | Run the `StateT` seeded with `s` and retrieve the resulting state.
execT ::
  Functor f =>
  StateT s f a
  -> s
  -> f s
execT StateT st =
  snd <$> st

-- | Run the `State` seeded with `s` and retrieve the resulting state.
exec' ::
  State' s a
  -> s
  -> s
exec' st = runId . execT st

-- | Run the `StateT` seeded with `s` and retrieve the resulting value.
evalT ::
  Functor f =>
  StateT s f a
  -> s
  -> f a
evalT StateT st =
  fst <$> st

-- | Run the `State` seeded with `s` and retrieve the resulting value.
eval' ::
  State' s a
  -> s
  -> a
eval' st = runId . evalT st

-- | A `StateT` where the state also distributes into the produced value.
--
-- >>> (runStateT (getT :: StateT Int List Int) 3)
-- [(3,3)]
getT ::
  Monad f =>
  StateT s f s
getT = StateT $ \s -> pure (s, s)

-- | A `StateT` where the resulting state is seeded with the given value.
--
-- >>> runStateT (putT 2) 0
-- ((),2)
--
-- >>> runStateT (putT 2 :: StateT Int List ()) 0
-- [((),2)]
putT ::
  Monad f =>
  s
  -> StateT s f ()
putT = StateT . const . pure . (,) ()

stateT :: (Monad m) =>
          m a
          -> (a -> s -> (b, s))
          -> StateT s m b
stateT ma f = let lifted = lift ma -- :: StateT s m a
              in lifted >>= \a -> StateT $ \s -> return (f a s)

notSeenYet' :: Ord a => a -> S.Set a -> (Bool, S.Set a)
notSeenYet' = runState . notSeenYet

-- | Remove all duplicate elements in a `List`.
--
-- /Tip:/ Use `filtering` and `State'` with a @Data.Set#Set@.
--
-- prop> distinct' xs == distinct' (flatMap (\x -> x :. x :. Nil) xs)
distinct' ::
  (Ord a, Num a) =>
  List a
  -> List a
distinct' = (`eval'` S.empty) . filtering f -- (wrapInStateT id' notSeenYet)
  where f a = stateT (Id a) notSeenYet'


-- | Remove all duplicate elements in a `List`.
-- However, if you see a value greater than `100` in the list,
-- abort the computation by producing `Empty`.
--
-- /Tip:/ Use `filtering` and `StateT` over `Optional` with a @Data.Set#Set@.
--
-- >>> distinctF $ listh [1,2,3,2,1]
-- Full [1,2,3]
--
-- >>> distinctF $ listh [1,2,3,2,1,101]
-- Empty
distinctF ::
  (Ord a, Num a) =>
  List a
  -> Optional (List a)
distinctF = (`evalT` S.empty) . filtering f
  where f a = stateT (check a) notSeenYet'
        check a = if a > 100
                  then Empty
                  else Full a

-- | An `OptionalT` is a functor of an `Optional` value.
data OptionalT f a =
  OptionalT {
    runOptionalT ::
      f (Optional a)
  }

-- | Implement the `Functor` instance for `OptionalT f` given a Functor f.
--
-- >>> runOptionalT $ (+1) <$> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty]
instance Functor f => Functor (OptionalT f) where
  (<$>) :: (a -> b)
           -> OptionalT f a
           -> OptionalT f b
  f <$> OptionalT a = let f' = (<$>) f -- :: Optional a -> Optional b
                      in OptionalT $ f' <$> a

-- | Implement the `Applicative` instance for `OptionalT f` given a Applicative f.
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty,Full 3,Empty]
instance Applicative f => Applicative (OptionalT f) where
  -- second pure is equivalent to Full
  pure =
    OptionalT . pure . pure

  (<*>) :: OptionalT f (a -> b)
           -> OptionalT f a
           -> OptionalT f b
  OptionalT f <*> OptionalT a =
    let -- ((<*>) <$>) :: (Functor f, Applicative g) =>
        --                f (g (a -> b)) -> f (g a -> g b)
        -- (<*>) <$> f -- :: f (Optional a -> Optional b)
    in OptionalT $ lift2 (<*>) f a -- (<*>) <$> f <*> a

-- -- | Implement the `Monad` instance for `OptionalT f` given a Monad f.
-- --
-- -- >>> runOptionalT $ (\a -> OptionalT (Full (a+1) :. Full (a+2) :. Nil)) =<< OptionalT (Full 1 :. Empty :. Nil)
-- -- [Full 2,Full 3,Empty]
instance Monad f => Monad (OptionalT f) where
  (=<<) :: (a -> OptionalT f b)
           -> OptionalT f a
           -> OptionalT f b
  f =<< OptionalT a =
    let -- runOptionalT . f -- :: a -> f (Optional b)
        -- f'' :: Optional a -> f (Optional b)
        f' (Full a) = runOptionalT . f $ a
        f' Empty    = pure Empty
    in OptionalT $ f' =<< a

instance MonadTrans OptionalT where
  -- lift :: Monad m => m a -> OptionalT m a
  lift ma = OptionalT $ ma >>= return . Full

optionalT :: Monad m =>
             m a
             -> (a -> Optional b)
             -> OptionalT m b
optionalT ma f = lift ma >>= OptionalT . return . f

-- | A `Logger` is a pair of a list of log values (`[l]`) and an arbitrary value (`a`).
data Logger l a =
  Logger (List l) a
  deriving (Eq, Show)

-- | Implement the `Functor` instance for `Logger
--
-- >>> (+3) <$> Logger (listh [1,2]) 3
-- Logger [1,2] 6
instance Functor (Logger l) where
  f <$> Logger l a = Logger l (f a)

-- | Implement the `Applicative` instance for `Logger`.
--
-- >>> pure "table" :: Logger Int P.String
-- Logger [] "table"
--
-- >>> Logger (listh [1,2]) (+7) <*> Logger (listh [3,4]) 3
-- Logger [1,2,3,4] 10
instance Applicative (Logger l) where
  pure = Logger Nil
  Logger fLog f <*> Logger aLog a =
    Logger (fLog ++ aLog) (f a)

-- | Implement the `Monad` instance for `Logger`.
-- The `bind` implementation must append log values to maintain associativity.
--
-- >>> (\a -> Logger (listh [4,5]) (a+3)) =<< Logger (listh [1,2]) 3
-- Logger [1,2,4,5] 6
instance Monad (Logger l) where
  makeLogger =<< Logger argLog arg =
    let Logger newEntries result = makeLogger arg
    in Logger (argLog ++ newEntries) result

-- | A utility function for producing a `Logger` with one log value.
--
-- >>> log1 1 2
-- Logger [1] 2
log1 ::
  l
  -> a
  -> Logger l a
log1 l = Logger (l :. Nil)

evalLog :: Logger l a -> a
evalLog (Logger _ a) = a

wrapInLog :: (Functor f) =>
             (a -> List b)
             -> (a -> StateT s f c)
             -> (a -> Logger b (StateT s f c))
wrapInLog f makeStateT a = Logger (f a) (makeStateT a)

-- | Remove all duplicate integers from a list. Produce a log as you go.
-- If there is an element above 100, then abort the entire computation and produce no result.
-- However, always keep a log. If you abort the computation, produce a log with the value,
-- "aborting > 100: " followed by the value that caused it.
-- If you see an even number, produce a log message, "even number: " followed by the even number.
-- Other numbers produce no log message.
--
-- /Tip:/ Use `filtering` and `StateT` over (`OptionalT` over `Logger` with a @Data.Set#Set@).
--
-- >>> distinctG $ listh [1,2,3,2,6]
-- Logger ["even number: 2","even number: 2","even number: 6"] (Full [1,2,3,6])
--
-- >>> distinctG $ listh [1,2,3,2,6,106]
-- Logger ["even number: 2","even number: 2","even number: 6","aborting > 100: 106"] Empty
distinctG ::
  (Integral a, Show a) =>
  List a
  -> Logger Chars (Optional (List a))
distinctG = runOptionalT . (`evalT` S.empty) . filtering f
  -- filtering f :: StateT (S.Set a) (OptionalT (Logger Chars)) (List a)
  -- (`evalT` S.empty) .. :: OptionalT (Logger Chars) (List a)
  -- runOptionalT      .. :: Logger Chars (Optional (List a))
  where f a = stateT (opt a) notSeenYet'
        opt a = optionalT (log a) check
        check a = if a > 100
                    then Empty
                    else Full a
        log a
          | a > 100 = log1 ("aborting > 100: " ++ show' a) a
          | even a  = log1 ("even number: " ++ show' a) a
          | otherwise = pure a

-- filtering :: Applicative f =>
--              (a -> f Bool)
--              -> List a
--              -> f (List a)
