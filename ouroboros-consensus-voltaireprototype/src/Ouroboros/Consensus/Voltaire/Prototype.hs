{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
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
  , Voltaire.VoltaireClass
  , Voltaire.VoltairePrototype(..)
  , Voltaire.VoltairePrototypeEra
  , VoltaireConstraints
  ) where

import           Ouroboros.Consensus.HardFork.Combinator

import           Ouroboros.Consensus.Shelley.Ledger

import           Ouroboros.Consensus.Voltaire.Prototype.Block

import           Cardano.Ledger.Voltaire.Prototype as Voltaire
import           Cardano.Ledger.Voltaire.Prototype.Class as Voltaire
import           Ouroboros.Consensus.Voltaire.Prototype.CanHardFork (VoltairePrototypeHardForkConstraints)

type ProtocolVoltairePrototype = HardForkProtocol '[ ShelleyBlock StandardShelley
                                                   , ShelleyBlock StandardVoltairePrototype
                                                   ]

type VoltaireConstraints proto c = VoltairePrototypeHardForkConstraints proto c
