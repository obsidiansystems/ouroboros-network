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
    -- * The block type of the Voltaire Prototype block chain
    VoltairePrototypeBlock
  , ProtocolVoltairePrototype
  ) where

import           Ouroboros.Consensus.HardFork.Combinator

import           Ouroboros.Consensus.Shelley.Ledger

import           Ouroboros.Consensus.Voltaire.Prototype.Block

type ProtocolVoltairePrototype = HardForkProtocol '[ ShelleyBlock StandardShelley
                                                   , ShelleyBlock StandardVoltairePrototype
                                                   ]
