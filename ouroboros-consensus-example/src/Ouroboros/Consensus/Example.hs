{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ouroboros.Consensus.Example (
    -- * The block type of the Example block chain
    ExampleBlock
  , ProtocolExample
  , Protocol(..)
  , RunProtocol (..)
  , RunProtocolClient(..)
  ) where

import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.Cardano (Protocol(..), ProtocolClient(..))

import           Ouroboros.Consensus.HardFork.Combinator

import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Node as X

import           Ouroboros.Consensus.Example.Block
import           Ouroboros.Consensus.Example.Node

type ProtocolExample = HardForkProtocol '[ ShelleyBlock StandardShelley
                                         , ShelleyBlock StandardExample
                                         ]

instance IOLike m => Protocol m (ExampleBlock StandardCrypto) ProtocolExample where
  data RunProtocol m (ExampleBlock StandardCrypto) ProtocolExample = RunProtocolExample
    (ProtocolParamsShelleyBased StandardShelley)
    ProtocolParamsShelley
    ProtocolParamsExample
    (ProtocolParamsTransition (ShelleyBlock StandardShelley) (ShelleyBlock StandardExample))
  protocolInfo (RunProtocolExample
                  paramsShelleyBased
                  paramsShelley
                  paramsExample
                  paramsShelleyExample) =
    protocolInfoExample
      paramsShelleyBased
      paramsShelley
      paramsExample
      paramsShelleyExample

instance ProtocolClient (ExampleBlock StandardCrypto) ProtocolExample where
  data RunProtocolClient (ExampleBlock StandardCrypto) ProtocolExample =
    RunProtocolClientExample
  protocolClientInfo RunProtocolClientExample =
    protocolClientInfoExample

