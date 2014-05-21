module Data.CExtraChan where

import Control.Applicative
import Data.IORef
import qualified Data.Sequence as S

data ExtraChan a = ExtraChan { chan :: IORef (S.Seq a)
                             , minEver :: IORef a
                             , maxEver :: IORef a
                             , lLength :: IORef Int
                             }

newExtraChan :: (Bounded a) => IO (ExtraChan a)
newExtraChan = ExtraChan
               <$> newIORef S.empty
               <*> newIORef maxBound
               <*> newIORef minBound
               <*> newIORef 0

writeExtraChan :: (Ord a) => ExtraChan a -> a -> IO ()
writeExtraChan c a = do
  modifyIORef (chan c) (S.|> a)
  modifyIORef (minEver c) $ \oldMin -> min a oldMin
  modifyIORef (maxEver c) $ \oldMax -> max a oldMax
  modifyIORef (lLength c) $ (+1)

readExtraChan :: ExtraChan a -> IO (Maybe a)
readExtraChan c = do
  v <- S.viewl <$> readIORef (chan c)
  a <- case v of
     (a S.:< as) -> do
       writeIORef (chan c) as
       modifyIORef (lLength c) pred
       return $ Just a
     S.EmptyL -> return Nothing
  return a

extraChanLength :: (Show a) => ExtraChan a -> IO Int
extraChanLength = readIORef . lLength

describe :: (Show a) => ExtraChan a -> IO String
describe s = do
  c    <- readIORef (chan s)
  min' <- readIORef (minEver s)
  max' <- readIORef (maxEver s)
  len' <- readIORef (lLength s)
  return $ unwords ["Chan:", show c
                   ,"min':", show min'
                   ,"max':", show max'
                   ,"len':", show len'
                   ]