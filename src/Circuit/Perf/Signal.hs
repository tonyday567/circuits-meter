{-# LANGUAGE BlockArguments #-}

-- | Three-signal control flow for Kleisli iteration on the @(,)@ tensor.
--
-- 'Signal' is a manual encoding of 'Either'-trace iteration semantics
-- (continue / exit) on the cartesian @(,)@ tensor.  The extra
-- 'Fallback' constructor gives alternation: @f '<|>' g@ tries @f@,
-- and if it signals 'Fallback', tries @g@ instead.
--
-- This bridges the two trace styles: 'Either' gives iteration for
-- free via 'Knot', but breaks when outputs are wrapped (e.g. by
-- 'meterK').  'Signal' rebuilds the same control flow by hand on
-- @(,)@, keeping the state type intact.  Because it works for any
-- @'Monad' m@, it applies to pure functions (@'Kleisli' 'Identity'@)
-- as well as effectful pipelines (@'Kleisli' 'IO'@).
module Circuit.Perf.Signal
  ( Signal (..),
    (<|>),
    loopAlt,
  )
where

import Control.Arrow (Kleisli (..))

-- | A three-signal branch: @Continue s@ loops with new state @s@,
--   @Fallback s@ tries the alternative in @('<|>')@, @Done r@ exits
--   the loop with result @r@.
data Signal s r = Continue s | Fallback s | Done r

-- | Try the first stage; if it signals 'Fallback', try the second.
--   'Continue' and 'Done' pass through unchanged.
infixl 3 <|>

(<|>) :: (Monad m) => Kleisli m s (Signal s r) -> Kleisli m s (Signal s r) -> Kleisli m s (Signal s r)
Kleisli f <|> Kleisli g = Kleisli \s -> do
  r <- f s
  case r of
    Continue s' -> pure (Continue s')
    Fallback s' -> g s'
    Done r' -> pure (Done r')
{-# INLINE (<|>) #-}

-- | Run a 'Signal'-producing step in a loop: feed 'Continue' back,
--   expect 'Fallback' to have been consumed by @('<|>')@, exit on 'Done'.
loopAlt :: (Monad m) => Kleisli m s (Signal s r) -> Kleisli m s r
loopAlt (Kleisli f) = Kleisli go
  where
    go s =
      f s >>= \case
        Continue s' -> go s'
        Fallback _ -> error "loopAlt: unexpected Fallback"
        Done r' -> pure r'
{-# INLINEABLE loopAlt #-}
