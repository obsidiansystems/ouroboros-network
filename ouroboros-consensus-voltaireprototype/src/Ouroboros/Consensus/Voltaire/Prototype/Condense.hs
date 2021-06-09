{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Voltaire.Prototype.Condense () where

import           Ouroboros.Consensus.HardFork.Combinator.Condense

import           Ouroboros.Consensus.Shelley.Ledger

import           Ouroboros.Consensus.Example.Block (ExampleBlock)
import           Ouroboros.Consensus.Voltaire.Prototype.CanHardFork

{-------------------------------------------------------------------------------
  Condense

  TODO where to put this?
-------------------------------------------------------------------------------}

instance ShelleyBasedEra era => CondenseConstraints (ShelleyBlock era)

instance VoltairePrototypeHardForkConstraints c => CondenseConstraints (ExampleBlock c)
