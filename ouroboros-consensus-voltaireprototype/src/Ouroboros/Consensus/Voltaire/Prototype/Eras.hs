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
  , StandardVoltairePrototype
    -- * Re-exports
  , EraCrypto
  , ShelleyBasedEra (..)
  , ShelleyEra
  , StandardCrypto
  , StandardShelley
  ) where

import           Cardano.Ledger.Voltaire.Prototype (VoltairePrototype(..), VoltairePrototypeEra)
import           Cardano.Ledger.Voltaire.Prototype.Class

import           Ouroboros.Consensus.Shelley.Eras
import qualified Shelley.Spec.Ledger.API as SL

{-------------------------------------------------------------------------------
  Eras instantiated with standard crypto
-------------------------------------------------------------------------------}

-- | The VoltairePrototype era with standard crypto
type StandardVoltairePrototype = VoltairePrototypeEra 'VoltairePrototype_One StandardCrypto

instance (SL.PraosCrypto c {-, PpupState(VoltairePrototypeEra proto c) ~ SL.PPupState (VoltairePrototypeEra proto c), VoltaireClass (VoltairePrototypeEra proto c) -}) => ShelleyBasedEra (VoltairePrototypeEra 'VoltairePrototype_One c) where
  shelleyBasedEraName _ = "VoltairePrototype"
