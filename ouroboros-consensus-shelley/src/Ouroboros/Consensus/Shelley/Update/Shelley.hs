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

-- | Functions for implementing 'Ouroboros.Consensus.Shelley.Update.HasProtocolUpdates'
--    for eras where @State (LC.EraRule "PPUP" era) ~ SL.PPUPState era@
module Ouroboros.Consensus.Shelley.Update.Shelley
( getProposedPPUpdates
, protocolUpdatesShelley
, exampleProposedProtocolUpdatesShelley
)
where

import           Cardano.Binary (toCBOR)
import qualified Shelley.Spec.Ledger.API as SL

import           Ouroboros.Consensus.Util (groupSplit)
import           Ouroboros.Consensus.Block ( EpochNo )
import           Ouroboros.Consensus.Ledger.Abstract ( LedgerState )
import qualified Cardano.Ledger.Shelley.Constraints as SL
import qualified Cardano.Ledger.Core as LC
import           Ouroboros.Consensus.Shelley.Eras (EraCrypto)
import           Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBasedEra, ShelleyBlock )

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Word (Word64)
import           Data.List (sortBy)
import           Data.Ord (comparing)
import           Data.Tuple (swap)


import Ouroboros.Consensus.Shelley.Ledger.Ledger
    ( LedgerState(shelleyLedgerState) )
import qualified Shelley.Spec.Ledger.PParams as SL (emptyPParamsUpdate, PParamsUpdate)

import           Cardano.Ledger.Crypto (ADDRHASH, Crypto)
import           Shelley.Spec.Ledger.API (StrictMaybe (..))
import qualified Shelley.Spec.Ledger.Keys as SL (hashWithSerialiser)
import           Cardano.Crypto.Hash (HashAlgorithm)
import qualified Cardano.Crypto.Hash as Hash


import           Cardano.Prelude (Proxy (Proxy))
import Data.Coerce (coerce)
import Control.State.Transition (STS(State))
import qualified Cardano.Ledger.Era

-- | Copied from 'Ouroboros.Consensus.Shelley.Ledger.Query'
getProposedPPUpdates ::
     (State (LC.EraRule "PPUP" era) ~ SL.PPUPState era)
  => SL.NewEpochState era -> SL.ProposedPPUpdates era
getProposedPPUpdates = SL.proposals . SL._ppups
                     . SL._utxoState . SL.esLState . SL.nesEs

-- | Copied from 'Ouroboros.Consensus.Shelley.Ledger.Inspect'
protocolUpdatesShelley ::
       forall era. (ShelleyBasedEra era, State (LC.EraRule "PPUP" era) ~ SL.PPUPState era)
    => SL.ShelleyGenesis era
    -> LedgerState (ShelleyBlock era)
    -> ( [(SL.PParamsDelta era, [SL.KeyHash 'SL.Genesis (Cardano.Ledger.Era.Crypto era)])]
       , Word64
       , EpochNo
       )
protocolUpdatesShelley genesis st =
   (proposalsInv, quorum, currentEpoch)
  where
    proposalsInv :: [(SL.PParamsDelta era, [SL.KeyHash 'SL.Genesis (EraCrypto era)])]
    proposalsInv =
          groupSplit id
        . sortBy (comparing fst)
        $ map swap (Map.toList proposals)

    -- Updated proposed within the proposal window
    proposals :: Map (SL.KeyHash 'SL.Genesis (EraCrypto era)) (SL.PParamsDelta era)
    SL.ProposedPPUpdates proposals =
          SL.proposals
        . SL._ppups
        . SL._utxoState
        . SL.esLState
        . SL.nesEs
        . shelleyLedgerState
        $ st

    -- A proposal is accepted if the number of votes is equal to or greater
    -- than the quorum. The quorum itself must be strictly greater than half
    -- the number of genesis keys, but we do not rely on that property here.
    quorum :: Word64
    quorum = SL.sgUpdateQuorum genesis

    -- The proposals in 'SL.proposals' are for the upcoming epoch
    -- (we ignore 'futureProposals')
    currentEpoch :: EpochNo
    currentEpoch = SL.nesEL . shelleyLedgerState $ st

-- | Copied from 'Test.Consensus.Shelley.Examples'
exampleProposedProtocolUpdatesShelley ::
  ( ShelleyBasedEra era
  , SL.PParamsDelta era ~ SL.PParamsUpdate era
  )
  => SL.ProposedPPUpdates era
exampleProposedProtocolUpdatesShelley = SL.ProposedPPUpdates $ Map.singleton
    (mkKeyHash 0)
    (SL.emptyPParamsUpdate {SL._keyDeposit = SJust (SL.Coin 100)})
 where
  mkKeyHash :: forall c discriminator. Crypto c => Int -> SL.KeyHash discriminator c
  mkKeyHash = SL.KeyHash . mkDummyHash (Proxy @(ADDRHASH c))
  mkDummyHash :: forall h a. HashAlgorithm h => Proxy h -> Int -> Hash.Hash h a
  mkDummyHash _ = coerce . SL.hashWithSerialiser @h toCBOR
