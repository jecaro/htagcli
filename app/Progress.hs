module Progress (connectWithProgress) where

import Data.Conduit.Combinators qualified as ConduitC
import Progress.Queue qualified as Queue
import System.Console.ANSI as ANSI
import UnliftIO.Async qualified as Async
import "conduit" Conduit ((.|))
import "conduit" Conduit qualified

-- | Given a source and a sink. Run the resulting conduit while displaying
-- progress messages if ran in an interactive terminal.
connectWithProgress ::
  Conduit.ConduitT () b (Conduit.ResourceT IO) () ->
  Conduit.ConduitT b Void (Conduit.ResourceT IO) a ->
  IO a
connectWithProgress conduitIn conduitOut =
  ifM
    (ANSI.hSupportsANSI stdout)
    (connectWithProgressInteractive conduitIn conduitOut)
    (Conduit.runConduitRes $ conduitIn .| conduitOut)

message :: Queue.Step a -> Text
message Queue.Step {..} =
  "Processing "
    <> show seCurrent
    <> " over "
    <> show seTotal
    <> " files..."

-- | To compute the progress, we run two concurrent conduits connected via a
-- queue.
-- - The first count the total number of items and push them to the queue.
-- - The second read from the queue, yields the items downstream, and display
-- the progress message.
connectWithProgressInteractive ::
  Conduit.ConduitT () b (Conduit.ResourceT IO) () ->
  Conduit.ConduitT b Void (Conduit.ResourceT IO) a ->
  IO a
connectWithProgressInteractive conduitIn conduitOut = do
  queue <- Queue.mkQueue
  result <-
    Async.concurrently
      -- This conduit fills up the queue with items. The queue keeps track of
      -- the total number of items pushed.
      ( do
          Conduit.runConduitRes $ conduitIn .| Conduit.mapM_C (Queue.push queue)
          Queue.done queue
      )
      -- This conduit reads from the queue, displays progress messages, and
      -- yields items downstream.
      ( do
          result <-
            Conduit.runConduitRes $
              ConduitC.repeatM (Queue.pop queue)
                .| ConduitC.mapWhile id
                .| Conduit.awaitForever
                  ( \step -> do
                      clearLineAndResetCursor

                      -- The downstream conduit can write to stdout as long as
                      -- it adds eol to each output
                      Conduit.yield $ Queue.seItem step

                      putText $ message step
                      hFlush stdout
                  )
                .| conduitOut
          clearLineAndResetCursor
          pure result
      )
  pure $ snd result

clearLineAndResetCursor :: (MonadIO m) => m ()
clearLineAndResetCursor = liftIO $ do
  ANSI.clearLine
  ANSI.setCursorColumn 0
