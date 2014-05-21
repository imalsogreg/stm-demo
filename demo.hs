module Main where

------------------------------------------------------------------------------
import Control.Applicative
import Control.Concurrent       (threadDelay)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM   (atomically)
import Control.Monad            (forM, replicateM_,replicateM)
import System.Environment       (getArgs)
import System.Random            (randomRIO)
import Text.Printf              (printf)
import Data.Foldable            (toList)
------------------------------------------------------------------------------
import qualified Data.MExtraChan as M
import qualified Data.TExtraChan as T
import qualified Data.CExtraChan as C

-- |Set the average interval between operations in a thread
rate :: Double
rate = 10000

-- |Set the number of operations done by threads doing the same thing
-- (If there are 2 reader threads, each will do (nOperations/2) reads)
nOperations :: Int
nOperations = 200

-- |Wrapper around MVar-based or TVar-based Fancy-Channels
type DemoChan a = Either (M.ExtraChan a) (T.ExtraChan a)

-- |Helper function - make a fast action take (jitter + 1/rate) time
jitterAction :: IO a -> IO a
jitterAction ma = do
  d <- randomRIO (-1,1)
  let delaySec = (10 ** d) / rate
  a <- ma
  threadDelay . floor $ 1000000 * delaySec
  return a

-- |Write n random data into a Fancy channel
exerciseWriter :: DemoChan Int -> ThreadName -> Int -> IO ()
exerciseWriter c tID n = replicateM_ n $ do
  v <- randomRIO (-1000000,1000000)
  jitterAction $ do
    case c of
      Left  m -> M.writeExtraChan m v
      Right t -> atomically $ T.writeExtraChan t v
    printf "Writer %s wrote %d\n" tID v

-- |Read n data from a Fancy Channel
exerciseReader :: DemoChan Int -> ThreadName -> Int -> IO ()
exerciseReader c tID n = replicateM_ n . jitterAction $ do
  v <- case c of
    Left  m -> M.readExtraChan m
    Right t -> atomically $ T.readExtraChan t
  printf "Reader %s read %d\n" tID v

-- |Get the length out of a Fancy channel at n timepoints
exerciseLength :: DemoChan Int -> ThreadName -> Int -> IO ()
exerciseLength c tID n = replicateM_ n . jitterAction $ do
  l <- case c of
    Left  m -> M.extraChanLength m
    Right t -> atomically $ T.extraChanLength t
  printf "Lengther %s saw length %d\n" tID l

-- |Exercise a Fancy Channel with nWrite, nRead, nLength threads
-- writing, reading, and taking length measurements
exercise :: DemoChan Int -> Int -> Int -> Int -> IO ()
exercise c nWrite nRead nLength = do
  ws <- forM (map show [1..nRead]) $ \name ->
    async $ exerciseWriter c name (nOperations `div` nRead)
    
  rs <- forM (map show [1..nWrite])  $ \name ->
    async $ exerciseReader c name (nOperations `div` nWrite)

  ls <- forM (map show [1..nLength]) $ \name ->
    async $ exerciseLength c name (nOperations `div` nLength)

  mapM_ wait (rs ++ ws ++ ls)

  case c of
    Left  m -> putStrLn =<< M.chanInfo m
    Right t -> putStrLn =<< atomically (T.chanInfo t)

main :: IO ()
main = do
  args <- getArgs
  (m,t,c) <- initChans
  case args of
    ["t", r, w, l] -> exercise  t (read r) (read w) (read l)  -- STM
    ["m", r, w, l] -> exercise  m (read r) (read w) (read l)  -- TVar  (mutex-like)
    ["c", r, w, l] -> exerciseC c (read r) (read w) (read l)  -- IORef
    []    -> exercise t 3 3 3 >> exercise m 3 3 3
    _     -> error "Usage: demo [m|t] nWriters nReaders nLengthers"

initChans :: IO (DemoChan Int, DemoChan Int,C.ExtraChan Int)
initChans = do
  m <- M.newExtraChan
  t <- atomically T.newExtraChan
  c <- C.newExtraChan
  return (Left m, Right t, c)

type ThreadName = String

------------------------------------------------------------------------------
-- |Just for fun - a completely not-threadsafe ExtraChan
exerciseC :: C.ExtraChan Int -> Int -> Int -> Int -> IO ()
exerciseC c nR nW nL = do
  ws <- forM [1..nW] $ \n -> async $
                             replicateM_ 200
                             (jitterAction (C.writeExtraChan c n) >>
                              putStrLn ("Wrote " ++ show n)
                             )

  rs <- replicateM nR . async $
        replicateM_ 200 (jitterAction
                         (C.readExtraChan c >>= print))

  ls <- replicateM nL . async $
        replicateM_ 200 (jitterAction
                         (C.extraChanLength c >>=
                          (putStrLn . ("Length: " ++) . show)))
  mapM_ wait (ws++rs++ls)
  putStrLn =<< C.describe c