module Data.TExtraChan where

import Control.Applicative
import Control.Concurrent.STM

data ExtraChan a = ExtraChan { tchan   :: TChan a
                             , minEver :: TVar a
                             , maxEver :: TVar a
                             , lLength :: TVar Int
                             }

newExtraChan :: (Bounded a) => STM (ExtraChan a)
newExtraChan = ExtraChan
          <$> newTChan
          <*> newTVar maxBound
          <*> newTVar minBound
          <*> newTVar 0

writeExtraChan :: Ord a => ExtraChan a -> a -> STM ()
writeExtraChan t a = do
  writeTChan (tchan t)   $ a
  modifyTVar (minEver t) $ \oldMin -> min a oldMin
  modifyTVar (maxEver t) $ \oldMax -> max a oldMax
  modifyTVar (lLength t) (+1)

readExtraChan :: ExtraChan a -> STM a
readExtraChan t = do
  a <- readTChan (tchan t)
  modifyTVar (lLength t) pred
  return a

extraChanLength :: ExtraChan a -> STM Int
extraChanLength = readTVar . lLength

chanInfo :: (Show a) => ExtraChan a -> STM String
chanInfo t = do
  min' <- readTVar $ minEver t
  max' <- readTVar $ maxEver t
  len' <- readTVar $ lLength t
  return $ unwords ["Min:",show min'
                   ,"Max:",show max'
                   ,"Len:",show len'
                   ]