module Data.MExtraChan where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Concurrent.Chan

data ExtraChan a = ExtraChan { chan    :: Chan a
                             , minEver :: MVar a
                             , maxEver :: MVar a
                             , lLength :: MVar Int
                             }

newExtraChan :: (Bounded a) => IO (ExtraChan a)
newExtraChan = ExtraChan
               <$> newChan
               <*> newMVar maxBound
               <*> newMVar minBound
               <*> newMVar 0

writeExtraChan :: (Ord a) => ExtraChan a -> a -> IO ()
writeExtraChan c a = do
  oldMin <- takeMVar (minEver c)
  oldMax <- takeMVar (maxEver c)
  oldLen <- takeMVar (lLength c)
  writeChan (chan c) a
  putMVar (lLength c) (succ oldLen)
  putMVar (maxEver c) (max oldMax a)
  putMVar (minEver c) (min oldMin a)
    
readExtraChan :: (Ord a) => ExtraChan a -> IO a
readExtraChan c = do
  len <- takeMVar (lLength c)
  a <- readChan $ chan c
  putMVar (lLength c) (len - 1)
  return a

readExtraChan' :: (Ord a) => ExtraChan a -> IO a
readExtraChan' c = do
  a <- readChan $ chan c
  l <- takeMVar (lLength c)
  putMVar (lLength c) (l - 1)
  return a


extraChanLength :: ExtraChan a -> IO Int
extraChanLength c = readMVar (lLength c)

chanInfo :: (Show a) => ExtraChan a -> IO String
chanInfo c = do
  xs   <- getChanContents (chan c)
  min' <- readMVar (minEver c)
  max' <- readMVar (maxEver c)
  len' <- readMVar (lLength c)
  return $ unwords ["Vals:", show xs
                   ,"Min:", show min'
                   ,"Max:", show max'
                   ,"Len:", show len'
                   ]