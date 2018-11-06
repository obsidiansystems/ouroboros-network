{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Logging (
    LogEvent -- opaque
  , loggerConsumer
  , showNetworkTraffic
  ) where

import           Control.Concurrent.STM
import           Control.Monad
import           Data.Semigroup ((<>))

import           Ouroboros.Consensus.Util
import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain (Chain (..))
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.ConsumersAndProducers
import           Ouroboros.Network.Node (NodeId)
import           Ouroboros.Network.ProtocolInterfaces
import           Ouroboros.Network.Testing.ConcreteBlock

data LogEvent = LogEvent {
    msg    :: String
  , sender :: NodeId
  }

showNetworkTraffic :: TBQueue LogEvent -> IO ()
showNetworkTraffic q = forever $ do
    LogEvent{..} <- atomically $ readTBQueue q
    putStrLn $ "[conv_with:" <> show sender <> "] " <> msg

instance Condense BlockHeader where
    condense BlockHeader{..} =
      "{hash: " <> condense headerHash
                <> ", blockNo: "
                <> condense headerBlockNo
                <> "}"

-- | Add logging to the example consumer
loggerConsumer :: forall block. (Condense [block], Condense block, HasHeader block)
               => TBQueue LogEvent
               -> TVar (Chain block)
               -> NodeId
               -> ConsumerHandlers block IO
loggerConsumer q chainvar ourProducer =
    addLogging $ exampleConsumer chainvar
  where
    addLogging :: ConsumerHandlers block IO -> ConsumerHandlers block IO
    addLogging c = ConsumerHandlers {
          getChainPoints = do
            pts <- getChainPoints c
            logMsg $ "getChainPoints, sending " <> show pts <> " to " <> show ourProducer
            return pts

        , addBlock = \b -> do
            logMsg $ "Received " <> condense b <> " from " <> show ourProducer
            addBlock c b
            logChain

        , rollbackTo = \p -> do
            logMsg $ "Rolling back to " <> show p
            rollbackTo c p
            logChain
        }

    logChain :: IO ()
    logChain = atomically $ do
        chain <- readTVar chainvar
        let m = "Current chain candidate: " <> condense (Chain.toOldestFirst chain)
        writeTBQueue q $ LogEvent m ourProducer

    logMsg :: String -> IO ()
    logMsg m = atomically $ writeTBQueue q $ LogEvent m ourProducer

{-------------------------------------------------------------------------------
  Orphans
-------------------------------------------------------------------------------}

deriving instance Condense BlockNo
deriving instance Condense ConcreteHeaderHash
