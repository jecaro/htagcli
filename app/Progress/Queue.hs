{- Intermediate structure for showing the progress between two conduits: one
 - that count the items, and the other that process them.
 -}
module Progress.Queue (Step (..), mkQueue, push, pop, done) where

import UnliftIO.STM qualified as STM

data Queue a = Queue
  { stTotal :: STM.TVar Int,
    stCurrent :: STM.TVar Int,
    stQueue :: STM.TQueue a,
    stDone :: STM.TVar Bool
  }

data Step a = Step
  { seItem :: a,
    seCurrent :: Int,
    seTotal :: Int
  }

mkQueue :: (MonadIO m) => m (Queue a)
mkQueue =
  Queue
    <$> STM.newTVarIO 0
    <*> STM.newTVarIO 0
    <*> STM.newTQueueIO
    <*> STM.newTVarIO False

done :: (MonadIO m) => Queue a -> m ()
done Queue {..} =
  STM.atomically $ STM.writeTVar stDone True

push :: (MonadIO m) => Queue a -> a -> m ()
push Queue {..} file = STM.atomically $ do
  STM.writeTQueue stQueue file
  STM.modifyTVar' stTotal (+ 1)

pop :: (MonadIO m) => Queue a -> m (Maybe (Step a))
pop Queue {..} = STM.atomically $ do
  isDone <- STM.readTVar stDone
  isEmpty <- STM.isEmptyTQueue stQueue
  if isDone && isEmpty
    then pure Nothing
    else do
      STM.modifyTVar' stCurrent (+ 1)
      seItem <- STM.readTQueue stQueue
      seCurrent <- STM.readTVar stCurrent
      seTotal <- STM.readTVar stTotal
      pure $ Just Step {..}
