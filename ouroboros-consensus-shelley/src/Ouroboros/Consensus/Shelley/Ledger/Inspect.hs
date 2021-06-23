{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Shelley.Ledger.Inspect (
    ProtocolUpdate (..)
  , ShelleyLedgerUpdate (..)
  , UpdateProposal (..)
  , UpdateState (..)
  , protocolUpdates
  ) where

import           Control.Monad
import           Data.Void

import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Util.Condense

import qualified Cardano.Ledger.Shelley.Constraints as SL
import qualified Shelley.Spec.Ledger.API as SL

import           Ouroboros.Consensus.Shelley.Ledger.Block
import           Ouroboros.Consensus.Shelley.Ledger.Ledger
import Ouroboros.Consensus.Shelley.Update 


{-------------------------------------------------------------------------------
  Inspection
-------------------------------------------------------------------------------}

data ShelleyLedgerUpdate era =
    ShelleyUpdatedProtocolUpdates [ProtocolUpdate era]

deriving instance Eq (SL.PParamsDelta era) => Eq (ShelleyLedgerUpdate era)
deriving instance Show (SL.PParamsDelta era) => Show (ShelleyLedgerUpdate era)

instance Show (SL.PParamsDelta era) => Condense (ShelleyLedgerUpdate era) where
  condense = show

instance (ShelleyBasedEra era, HasProtocolUpdates era) => InspectLedger (ShelleyBlock era) where
  type LedgerWarning (ShelleyBlock era) = Void
  type LedgerUpdate  (ShelleyBlock era) = ShelleyLedgerUpdate era

  inspectLedger tlc before after = do
      guard $ updatesBefore /= updatesAfter
      return $ LedgerUpdate $ ShelleyUpdatedProtocolUpdates updatesAfter
    where
      genesis :: SL.ShelleyGenesis era
      genesis = shelleyLedgerGenesis (configLedger tlc)

      updatesBefore, updatesAfter :: [ProtocolUpdate era]
      updatesBefore = protocolUpdates genesis before
      updatesAfter  = protocolUpdates genesis after
