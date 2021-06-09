{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ouroboros.Consensus.Voltaire.Prototype (
    -- * The block type of the Example block chain
    VoltairePrototypeBlock
  , ProtocolExample
  ) where

import           Ouroboros.Consensus.HardFork.Combinator

import           Ouroboros.Consensus.Shelley.Ledger

import           Ouroboros.Consensus.Voltaire.Prototype.Block

type ProtocolExample = HardForkProtocol '[ ShelleyBlock StandardShelley
                                         , ShelleyBlock StandardVoltairePrototype
                                         ]
