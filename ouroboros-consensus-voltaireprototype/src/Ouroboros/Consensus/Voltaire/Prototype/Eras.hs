{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ouroboros.Consensus.Voltaire.Prototype.Eras (
    -- * Eras based on the Shelley ledger
    VoltairePrototypeEra
    -- * Eras instantiated with standard crypto
  , StandardVoltairePrototypeTwo
    -- * Re-exports
  , VoltairePrototype(..)
  , EraCrypto
  , ShelleyBasedEra (..)
  , ShelleyEra
  , StandardCrypto
  , StandardShelley
  ) where

import           Cardano.Ledger.Voltaire.Prototype (VoltairePrototype(..), VoltairePrototypeEra)

import qualified Ouroboros.Consensus.Shelley.Update.Shelley as Shelley
import qualified Ouroboros.Consensus.Voltaire.Prototype.Update as Two
import           Ouroboros.Consensus.Shelley.Eras
import           Ouroboros.Consensus.Shelley.Update
    (protocolUpdatesShelley, HasProtocolUpdates(..))
import qualified Shelley.Spec.Ledger.API as SL

{-------------------------------------------------------------------------------
  Eras instantiated with standard crypto
-------------------------------------------------------------------------------}

-- | The VoltairePrototype era with standard crypto
type StandardVoltairePrototypeTwo = VoltairePrototypeEra 'VoltairePrototype_Two StandardCrypto

instance (SL.PraosCrypto c) => ShelleyBasedEra (VoltairePrototypeEra 'VoltairePrototype_One c) where
  shelleyBasedEraName _ = "VoltairePrototypeOne"
instance (SL.PraosCrypto c) => ShelleyBasedEra (VoltairePrototypeEra 'VoltairePrototype_Two c) where
  shelleyBasedEraName _ = "VoltairePrototypeTwo"

instance SL.PraosCrypto c => SL.ShelleyBasedEra (VoltairePrototypeEra 'VoltairePrototype_One c)
instance SL.PraosCrypto c => SL.ApplyBlock (VoltairePrototypeEra 'VoltairePrototype_One c)
instance SL.PraosCrypto c => SL.ApplyTx (VoltairePrototypeEra 'VoltairePrototype_One c)
instance SL.PraosCrypto c => SL.GetLedgerView (VoltairePrototypeEra 'VoltairePrototype_One c)

instance SL.PraosCrypto c => SL.ShelleyBasedEra (VoltairePrototypeEra 'VoltairePrototype_Two c)
instance SL.PraosCrypto c => SL.ApplyBlock (VoltairePrototypeEra 'VoltairePrototype_Two c)
instance SL.PraosCrypto c => SL.ApplyTx (VoltairePrototypeEra 'VoltairePrototype_Two c)
instance SL.PraosCrypto c => SL.GetLedgerView (VoltairePrototypeEra 'VoltairePrototype_Two c)

instance SL.PraosCrypto c => HasProtocolUpdates (VoltairePrototypeEra 'VoltairePrototype_One c) where
  type ProposedProtocolUpdates (VoltairePrototypeEra 'VoltairePrototype_One c)
    = SL.ProposedPPUpdates (VoltairePrototypeEra 'VoltairePrototype_One c)
  protocolUpdates genesis st =
    let (proposalsInv, quorum, currentEpoch) = Shelley.protocolUpdatesShelley genesis st
    in protocolUpdatesShelley proposalsInv quorum currentEpoch
  getProposedProtocolUpdates = Shelley.getProposedPPUpdates
  exampleProposedProtocolUpdates _ = Shelley.exampleProposedProtocolUpdatesShelley

instance SL.PraosCrypto c => HasProtocolUpdates (VoltairePrototypeEra 'VoltairePrototype_Two c) where
  type ProposedProtocolUpdates (VoltairePrototypeEra 'VoltairePrototype_Two c)
    = Two.ProposedUpdates (VoltairePrototypeEra 'VoltairePrototype_Two c)
  protocolUpdates genesis st =
    let (proposalsInv, quorum, currentEpoch) = Two.protocolUpdates genesis st
    in protocolUpdatesShelley proposalsInv quorum currentEpoch
  getProposedProtocolUpdates = Two.getProposedUpdates
  exampleProposedProtocolUpdates _ = Two.exampleProposedProtocolUpdatesShelley
