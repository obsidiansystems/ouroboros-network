{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Ouroboros.Consensus.Shelley.Update
( HasProtocolUpdates(..)
, ProtocolUpdate(..)
, UpdateProposal(..)
, UpdateState(..)
, protocolUpdatesShelley
)
where

import qualified Ouroboros.Consensus.Shelley.Update.Shelley as Shelley
import           Data.Kind (Type)
import           Cardano.Binary (ToCBOR, FromCBOR)
import qualified Shelley.Spec.Ledger.API as SL

import           Ouroboros.Consensus.Block ( EpochNo )
import           Ouroboros.Consensus.Ledger.Abstract ( LedgerState )
import qualified Cardano.Ledger.Shelley.Constraints as SL
import qualified Cardano.Ledger.Core as LC
import           Ouroboros.Consensus.Shelley.Eras (EraCrypto)
import           Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBasedEra, ShelleyBlock )

import           GHC.Records ( HasField(getField) )

import           Shelley.Spec.Ledger.BaseTypes (strictMaybeToMaybe)

import           Cardano.Ledger.Allegra (AllegraEra)
import           Cardano.Ledger.Mary (MaryEra)
import           Cardano.Ledger.Shelley (ShelleyEra)
import           Cardano.Prelude (Proxy)
import           Control.State.Transition (STS(State))

data ProtocolUpdate era = ProtocolUpdate {
      protocolUpdateProposal :: UpdateProposal era
    , protocolUpdateState    :: UpdateState (EraCrypto era)
    }
deriving instance Eq (SL.PParamsDelta era) => Eq (ProtocolUpdate era)
deriving instance Show (SL.PParamsDelta era) => Show (ProtocolUpdate era)

-- | Update proposal
--
-- As in Byron, a proposal is a partial map from parameters to their values.
data UpdateProposal era = UpdateProposal {
      -- | The protocol parameters changed by this update proposal
      --
      -- An update is /identified/ by how it updates the protocol parameters.
      proposalParams  :: SL.PParamsDelta era

      -- | New version (if changed by this proposal)
      --
      -- The protocol version itself is also considered to be just another
      -- parameter, and parameters can change /without/ changing the protocol
      -- version, although a convention /could/ be established that the protocol
      -- version must change if any of the parameters do; but the specification
      -- itself does not mandate this.
      --
      -- We record the version separately for the convenience of the HFC.
    , proposalVersion :: Maybe SL.ProtVer

      -- | The 'EpochNo' the proposal becomes active in, if it is adopted
    , proposalEpoch   :: EpochNo
    }

deriving instance Eq (SL.PParamsDelta era) => Eq (UpdateProposal era)
deriving instance Show (SL.PParamsDelta era) => Show (UpdateProposal era)

-- | Proposal state
--
-- The update mechanism in Shelley is simpler than it is in Byron. There is no
-- distinction between votes and proposals: to \"vote\" for a proposal one
-- merely submits the exact same proposal. There is also no separate
-- endorsement step. The procedure is as follows:
--
-- 1. During each epoch, a genesis key can submit (via its delegates) zero,
--    one, or many proposals; each submission overrides the previous one.
-- 2. \"Voting\" (submitting of proposals) ends @2 * stabilityWindow@ slots
--    (i.e. @6k/f@) before the end of the epoch. In other words, proposals
--    for the upcoming epoch must be submitted within the first @4k/f@ slots
--    of this one.
-- 3. At the end of an epoch, if the majority of nodes (as determined by the
--    @Quorum@ specification constant, which must be greater than half the
--    nodes) have most recently submitted the same exact proposal, then it is
--    adopted.
-- 4. The next epoch is always started with a clean slate, proposals from the
--    previous epoch that didn't make it are discarded (except for "future
--    proposals" that are explicitly marked for future epochs).
data UpdateState c = UpdateState {
      -- | The genesis delegates that voted for this proposal
      proposalVotes         :: [SL.KeyHash 'SL.Genesis c]

      -- | Has this proposal reached sufficient votes to be adopted?
    , proposalReachedQuorum :: Bool
    }
  deriving (Show, Eq)

-- |
class ( Eq (ProposedProtocolUpdates era)
      , Show (ProposedProtocolUpdates era)
      , ToCBOR (ProposedProtocolUpdates era)
      , FromCBOR (ProposedProtocolUpdates era)
      ) => HasProtocolUpdates era where
  type ProposedProtocolUpdates era :: Type

  protocolUpdates ::
       SL.ShelleyGenesis era
    -> LedgerState (ShelleyBlock era)
    -> [ProtocolUpdate era]

  getProposedProtocolUpdates ::
      SL.NewEpochState era -> ProposedProtocolUpdates era

  -- | Used by 'Test.Consensus.Shelley.Examples'
  exampleProposedProtocolUpdates :: Proxy era -> ProposedProtocolUpdates era

instance SL.PraosCrypto c => HasProtocolUpdates (ShelleyEra c) where
  type ProposedProtocolUpdates (ShelleyEra c) = SL.ProposedPPUpdates (ShelleyEra c)
  protocolUpdates = protocolUpdatesShelley
  getProposedProtocolUpdates = Shelley.getProposedPPUpdates
  exampleProposedProtocolUpdates _ = Shelley.exampleProposedProtocolUpdatesShelley

instance (SL.PraosCrypto c) => HasProtocolUpdates (AllegraEra c) where
  type ProposedProtocolUpdates (AllegraEra c) = SL.ProposedPPUpdates (AllegraEra c)
  protocolUpdates = protocolUpdatesShelley
  getProposedProtocolUpdates = Shelley.getProposedPPUpdates
  exampleProposedProtocolUpdates _ = Shelley.exampleProposedProtocolUpdatesShelley

instance SL.PraosCrypto c => HasProtocolUpdates (MaryEra c) where
  type ProposedProtocolUpdates (MaryEra c) = SL.ProposedPPUpdates (MaryEra c)
  protocolUpdates = protocolUpdatesShelley
  getProposedProtocolUpdates = Shelley.getProposedPPUpdates
  exampleProposedProtocolUpdates _ = Shelley.exampleProposedProtocolUpdatesShelley

protocolUpdatesShelley ::
       forall era. (ShelleyBasedEra era, State (LC.EraRule "PPUP" era) ~ SL.PPUPState era)
    => SL.ShelleyGenesis era
    -> LedgerState (ShelleyBlock era)
    -> [ProtocolUpdate era]
protocolUpdatesShelley genesis st = [
      ProtocolUpdate {
          protocolUpdateProposal = UpdateProposal {
              proposalParams  = proposal
            , proposalEpoch   = succ currentEpoch
            , proposalVersion = strictMaybeToMaybe $
                                  getField @"_protocolVersion" proposal
            }
        , protocolUpdateState = UpdateState {
              proposalVotes         = votes
            , proposalReachedQuorum = length votes >= fromIntegral quorum
            }
        }
    | (proposal, votes) <- proposalsInv
    ]
  where
    (proposalsInv, quorum, currentEpoch) = Shelley.protocolUpdatesShelley genesis st
