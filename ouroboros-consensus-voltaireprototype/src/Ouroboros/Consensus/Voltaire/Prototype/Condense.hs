{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Voltaire.Prototype.Condense () where

import           Ouroboros.Consensus.HardFork.Combinator.Condense

import           Ouroboros.Consensus.Shelley.Ledger

import           Ouroboros.Consensus.Voltaire.Prototype.Block (VoltairePrototypeBlock)
import           Shelley.Spec.Ledger.API.Protocol (PraosCrypto)

{-------------------------------------------------------------------------------
  Condense

  TODO where to put this?
-------------------------------------------------------------------------------}

instance ShelleyBasedEra era => CondenseConstraints (ShelleyBlock era)

instance PraosCrypto c => CondenseConstraints (VoltairePrototypeBlock c)
