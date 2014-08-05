{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main (main) where

import Control.Concurrent (forkIO, killThread, threadDelay, ThreadId)
import Control.Concurrent.Chan (newChan, readChan, writeChan, Chan)
import Control.Concurrent.MVar (modifyMVar, newMVar, takeMVar, putMVar, MVar)

main :: IO ()
main = do
  putStrLn "minicron"
  wakeupService makeTasks

wakeupService client = do
  chan <- newChan
  let request g n = writeChan chan $ RequestWakeUp g n
      cancel = writeChan chan CancelWakeUp
  tasks <- client request cancel
  mainThread chan tasks Nothing

mainThread :: Chan Event -> Client -> Maybe ThreadId -> IO ()
mainThread chan tasks msleep = do
  ev <- readChan chan
  case ev of
    WakeUp g -> do
      mw <- wakeup tasks g
      mainThread' chan mw tasks

    RequestWakeUp g amount -> do
      putStrLn "Wakeup requested."
      maybe (return ()) killThread msleep
      mainThread' chan (Just (g, amount)) tasks

    CancelWakeUp -> do
      putStrLn "Canceling wakeup."
      maybe (return ()) killThread msleep
      mainThread' chan Nothing tasks

mainThread' chan mw tasks = do
  msleep <- maybe (return Nothing) (fmap Just . forkIO . sleepThread chan) mw
  mainThread chan tasks msleep

sleepThread chan (g, amount) = do
  threadDelay $ amount * 1000000
  writeChan chan $ WakeUp g

data Event =
    WakeUp Generation
  -- ^ Sent by the sleep thread for a particular generation.
  | RequestWakeUp Generation Int
  -- ^ Cancel any previously requested wakeup, and request another one.
  -- First the generation, then the amount of time in seconds to wait.
  | CancelWakeUp
  -- ^ Cancel any previously requested wakeup.

-- | A generation is used to identify a particular state in the tasks thread.
-- It can be viewed as a request ID from the client to the wakeup service. The
-- wakeup service will answer with it, allowing the client to discard spurious
-- wakeups.
newtype Generation = Generation Int
  deriving (Eq, Num)

data Client = Client { wakeup :: Generation -> IO (Maybe (Generation, Int)) }

-- In this simple cron modelling, this is the number of seconds to wait
-- after the previous task has been run (or the system has been started).
type Task = Int

data Tasks = Tasks (MVar (Generation, [Task]))
  -- ^ Generation (used to discard WakeUps from explicitely killed sleep),
  -- set of tasks.

-- | Get a reference to tasks. Doing so, let them communicate asynchronously
-- if a new wakeup must be rescheduled, or completely canceled.
makeTasks :: (Generation -> Int -> IO ()) -> (IO ()) -> IO Client
makeTasks request cancel = do
  ref <- newMVar (0, [5, 6, 7, 8])
  _ <- forkIO $ do
    request 0 5
    threadDelay $ 13 * 1000000
    g <- modifyMVar ref (\(g, _) -> return ((g + 1, [1, 2]), g))
    request (g + 1) 1
    threadDelay $ 10 * 1000000
    g' <- modifyMVar ref (\(g, _) -> return ((g + 1, [1, 2, 2, 2, 2, 2]), g))
    request (g' + 1) 1
    threadDelay $ 6 * 1000000
    cancel
  return . Client . runTasks $ Tasks ref

-- | Give a chance to tasks to be run. Return possibly a request to receive
-- another wakeup n seconds later. The second argument is a generation (i.e. a
-- identifier to discard wakeups received while they were cancelled).
-- To request a wakeup without waiting for `runTasks` to be called, the
-- `request` argument in `makeTasks` above should be used.
runTasks :: Tasks -> Generation -> IO (Maybe (Generation, Int))
runTasks (Tasks ref) g = do
  (g', tasks) <- takeMVar ref
  case tasks of
    _ | g /= g' -> do
      -- This is a wakeup from a cancel request. Ignore it.
      putMVar ref (g', tasks)
      return Nothing
    [] -> do
      putStrLn $ "Error: no task after wakeup."
      putMVar ref (g', tasks)
      return Nothing
    task : tasks' -> do
      putStrLn $ "Running task: " ++ show task
      putMVar ref (g + 1, tasks')
      case tasks' of
        [] -> return Nothing
        amount:_ -> return $ Just (g + 1, amount)
