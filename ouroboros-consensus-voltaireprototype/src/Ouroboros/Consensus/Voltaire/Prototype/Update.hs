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
--    for eras where @State (LC.EraRule "PPUP" era) ~ Two.PPUPState era@
module Ouroboros.Consensus.Voltaire.Prototype.Update
( getProposedUpdates
, protocolUpdates
, exampleProposedProtocolUpdatesVoltaire
, Two.ProposedUpdates
)
where

import           Cardano.Binary (toCBOR)
import qualified Shelley.Spec.Ledger.API as SL

import           Ouroboros.Consensus.Util (groupSplit)
import           Ouroboros.Consensus.Block ( EpochNo )
import           Ouroboros.Consensus.Ledger.Abstract ( LedgerState )
import qualified Cardano.Ledger.Voltaire.Prototype.Two as Two
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

import qualified Cardano.Ledger.Era
import Control.State.Transition.Extended (STS(State))

getProposedUpdates ::
     (State (LC.EraRule "PPUP" era) ~ Two.PPUPState era) =>
     SL.NewEpochState era -> Two.ProposedUpdates era
getProposedUpdates = Two.proposals . SL._ppups
                     . SL._utxoState . SL.esLState . SL.nesEs

protocolUpdates ::
       forall era. (ShelleyBasedEra era, (State (LC.EraRule "PPUP" era) ~ Two.PPUPState era))
    => SL.ShelleyGenesis era
    -> LedgerState (ShelleyBlock era)
    -> ( [(SL.PParamsDelta era, [SL.KeyHash 'SL.Genesis (Cardano.Ledger.Era.Crypto era)])]
       , Word64
       , EpochNo
       )
protocolUpdates genesis st =
   (proposalsInv, quorum, currentEpoch)
  where
    proposalsInv :: [(SL.PParamsDelta era, [SL.KeyHash 'SL.Genesis (EraCrypto era)])]
    proposalsInv =
          groupSplit id
        . sortBy (comparing fst)
        $ map swap (Map.toList $ Map.mapMaybe Two.bodyPParamsDelta proposals)

    -- Updated proposed within the proposal window
    proposals :: Map (SL.KeyHash 'SL.Genesis (EraCrypto era)) (Two.ProposalBody era)
    Two.ProposedUpdates proposals =
          getProposedUpdates
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

-- | TODO: add a 'Two.BodyMIR' value as well.
--   See 'Test.Shelley.Spec.Ledger.Examples.Mir.txbodyEx1'
--   for an example of this.
exampleProposedProtocolUpdatesVoltaire ::
  ( ShelleyBasedEra era
  , SL.PParamsDelta era ~ SL.PParamsUpdate era
  )
  => Two.ProposedUpdates era
exampleProposedProtocolUpdatesVoltaire =
  Two.ProposedUpdates $ fmap Two.BodyPPUP map'
 where
  map' = Map.singleton
    (mkKeyHash 0)
    (SL.emptyPParamsUpdate {SL._keyDeposit = SJust (SL.Coin 100)})
  mkKeyHash :: forall c discriminator. Crypto c => Int -> SL.KeyHash discriminator c
  mkKeyHash = SL.KeyHash . mkDummyHash (Proxy @(ADDRHASH c))
  mkDummyHash :: forall h a. HashAlgorithm h => Proxy h -> Int -> Hash.Hash h a
  mkDummyHash _ = coerce . SL.hashWithSerialiser @h toCBOR
