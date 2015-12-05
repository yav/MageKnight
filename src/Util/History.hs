{-# LANGUAGE Safe, RecordWildCards #-}
module Util.History
  ( History
  , history
  , historyCurrent
  , historyUndo
  , historyRedo
  , historyUpdate
  , historyUpdateF
  , historyForget
  ) where

-- | Keep track of how a value changes.
data History a = History
  { historyPast   :: [a]
  , historyNow    :: a
  , historyFuture :: [a]
  }

-- | A new value with no past.
history :: a -> History a
history a = History { historyPast = [], historyNow = a, historyFuture = [] }

-- | Modify a value, saving the old value in the history.
-- This "forks" the timeline, which means that any future values
-- are forgotten and we cannot redo previusly undone actions.
historyUpdate :: (a -> a) -> History a -> History a
historyUpdate f History { .. } =
  History { historyPast   = historyNow : historyPast
          , historyNow    = f historyNow
          , historyFuture = []
          }

-- | Modify a value in the context of a functor.
-- The old value is saved in the history.
-- This "forks" the timeline, which means that any future values
-- are forgotten and we cannot redo previusly undone actions.
historyUpdateF :: Functor f => (a -> f a) -> History a -> f (History a)
historyUpdateF f History { .. } = fmap rebuild (f historyNow)
  where rebuild a = History { historyPast   = historyNow : historyPast
                            , historyNow    = a
                            , historyFuture = []
                            }

-- | Revert to a previous value.  The current value is saved, so that
-- we can "redo" later.
historyUndo :: History a -> Maybe (History a)
historyUndo History { .. } =
  case historyPast of
    []     -> Nothing
    a : as -> Just History { historyPast   = as
                           , historyNow    = a
                           , historyFuture = historyNow : historyFuture
                           }

-- | Redo one previously undone action.
historyRedo :: History a -> Maybe (History a)
historyRedo History { .. } =
  case historyFuture of
    []     -> Nothing
    a : as -> Just History { historyPast   = historyNow : historyPast
                           , historyNow    = a
                           , historyFuture = as
                           }

-- | The current value.
historyCurrent :: History a -> a
historyCurrent History { .. } = historyNow

-- | Forget the history.  We cannot undo after this point.
historyForget :: History a -> History a
historyForget History { .. } = History { historyPast = [], .. }



