{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Shelley.ShelleyHFC (
    ShelleyBlockHFC
  , ShelleyPartialLedgerConfig (..)
  , ProtocolShelley
  , RunProtocol(..)
  , RunProtocolClient(..)
  ) where

import           Control.Monad (guard)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.SOP.Strict
import           Data.Word
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Cardano.Slotting.Slot (EpochNo(..))

import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Protocol
import           Ouroboros.Consensus.Node.NetworkProtocolVersion

import           Ouroboros.Consensus.HardFork (TriggerHardFork(..))
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Embed.Unary
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common

import           Ouroboros.Consensus.Shelley.Eras
import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Ledger.Inspect as Shelley.Inspect
import           Ouroboros.Consensus.Shelley.Node (ProtocolParamsShelley, ProtocolParamsShelleyBased, protocolClientInfoShelley, protocolInfoShelley)
import           Ouroboros.Consensus.Shelley.Protocol

import           Ouroboros.Consensus.Util.IOLike (IOLike)

import qualified Shelley.Spec.Ledger.API as SL

-- import           Ouroboros.Consensus.Cardano.Node ()

{-------------------------------------------------------------------------------
  Synonym for convenience
-------------------------------------------------------------------------------}

-- | Shelley as the single era in the hard fork combinator
type ShelleyBlockHFC era = HardForkBlock '[ShelleyBlock era]

{-------------------------------------------------------------------------------
  NoHardForks instance
-------------------------------------------------------------------------------}

instance ShelleyBasedEra era => NoHardForks (ShelleyBlock era) where
  getEraParams =
        shelleyEraParamsNeverHardForks
      . shelleyLedgerGenesis
      . configLedger
  toPartialConsensusConfig _  = tpraosParams
  toPartialLedgerConfig _ cfg = ShelleyPartialLedgerConfig {
        shelleyLedgerConfig    = cfg
      , shelleyTriggerHardFork = TriggerHardForkNever
      }

{-------------------------------------------------------------------------------
  SupportedNetworkProtocolVersion instance
-------------------------------------------------------------------------------}

-- | Forward to the ShelleyBlock instance. Only supports
-- 'HardForkNodeToNodeDisabled', which is compatible with nodes running with
-- 'ShelleyBlock'.
instance ShelleyBasedEra era
      => SupportedNetworkProtocolVersion (ShelleyBlockHFC era) where
  supportedNodeToNodeVersions _ =
      Map.map HardForkNodeToNodeDisabled $
      supportedNodeToNodeVersions (Proxy @(ShelleyBlock era))

  supportedNodeToClientVersions _ =
      Map.map HardForkNodeToClientDisabled $
      supportedNodeToClientVersions (Proxy @(ShelleyBlock era))

{-------------------------------------------------------------------------------
  SerialiseHFC instance
-------------------------------------------------------------------------------}

-- | Use the default implementations. This means the serialisation of blocks
-- includes an era wrapper. Each block should do this from the start to be
-- prepared for future hard forks without having to do any bit twiddling.
instance ShelleyBasedEra era => SerialiseHFC '[ShelleyBlock era]
instance ShelleyBasedEra era => SerialiseConstraintsHFC (ShelleyBlock era)

{-------------------------------------------------------------------------------
  Protocol Instances
-------------------------------------------------------------------------------}

type ProtocolShelley = HardForkProtocol '[ ShelleyBlock StandardShelley ]

-- | Run TPraos against the Shelley ledger
instance IOLike m => Protocol m (ShelleyBlockHFC StandardShelley) ProtocolShelley where
  data RunProtocol m (ShelleyBlockHFC StandardShelley) ProtocolShelley = RunProtocolShelley
    (ProtocolParamsShelleyBased StandardShelley)
    (ProtocolParamsShelley)
  protocolInfo (RunProtocolShelley paramsShelleyBased paramsShelley) =
    inject $ protocolInfoShelley paramsShelleyBased paramsShelley

instance ProtocolClient (ShelleyBlockHFC StandardShelley) ProtocolShelley where
  data RunProtocolClient (ShelleyBlockHFC StandardShelley) ProtocolShelley =
    RunProtocolClientShelley
  protocolClientInfo RunProtocolClientShelley =
    inject $ protocolClientInfoShelley

{-------------------------------------------------------------------------------
  SingleEraBlock Shelley
-------------------------------------------------------------------------------}

shelleyTransition ::
     forall era. ShelleyBasedEra era
  => PartialLedgerConfig (ShelleyBlock era)
  -> Word16   -- ^ Next era's major protocol version
  -> LedgerState (ShelleyBlock era)
  -> Maybe EpochNo
shelleyTransition ShelleyPartialLedgerConfig{..}
                  transitionMajorVersion
                  state =
      takeAny
    . mapMaybe isTransition
    . Shelley.Inspect.protocolUpdates genesis
    $ state
  where
    ShelleyTransitionInfo{..} = shelleyLedgerTransition state

    -- 'shelleyLedgerConfig' contains a dummy 'EpochInfo' but this does not
    -- matter for extracting the genesis config
    genesis :: SL.ShelleyGenesis era
    genesis = shelleyLedgerGenesis shelleyLedgerConfig

    k :: Word64
    k = SL.sgSecurityParam genesis

    isTransition :: Shelley.Inspect.ProtocolUpdate era -> Maybe EpochNo
    isTransition Shelley.Inspect.ProtocolUpdate{..} = do
         SL.ProtVer major _minor <- proposalVersion
         guard $ fromIntegral major == transitionMajorVersion
         guard $ proposalReachedQuorum
         guard $ shelleyAfterVoting >= fromIntegral k
         return proposalEpoch
       where
         Shelley.Inspect.UpdateProposal{..} = protocolUpdateProposal
         Shelley.Inspect.UpdateState{..}    = protocolUpdateState

    -- In principle there could be multiple proposals that all change the
    -- major protocol version. In practice this can't happen because each
    -- delegate can only vote for one proposal, but the types don't guarantee
    -- this. We don't need to worry about this, and just pick any of them.
    takeAny :: [a] -> Maybe a
    takeAny = listToMaybe

instance ShelleyBasedEra era => SingleEraBlock (ShelleyBlock era) where
  singleEraTransition pcfg _eraParams _eraStart ledgerState =
      case shelleyTriggerHardFork pcfg of
        TriggerHardForkNever                         -> Nothing
        TriggerHardForkAtEpoch   epoch               -> Just epoch
        TriggerHardForkAtVersion shelleyMajorVersion ->
            shelleyTransition
              pcfg
              shelleyMajorVersion
              ledgerState

  singleEraInfo _ = SingleEraInfo {
      singleEraName = shelleyBasedEraName (Proxy @era)
    }

instance PraosCrypto c => HasPartialConsensusConfig (TPraos c) where
  type PartialConsensusConfig (TPraos c) = TPraosParams

  completeConsensusConfig _ tpraosEpochInfo tpraosParams = TPraosConfig {..}

data ShelleyPartialLedgerConfig era = ShelleyPartialLedgerConfig {
      -- | We cache the non-partial ledger config containing a dummy
      -- 'EpochInfo' that needs to be replaced with the correct one.
      --
      -- We do this to avoid recomputing the ledger config each time
      -- 'completeLedgerConfig' is called, as 'mkShelleyLedgerConfig' does
      -- some rather expensive computations that shouldn't be repeated too
      -- often (e.g., 'sgActiveSlotCoeff').
      shelleyLedgerConfig    :: !(ShelleyLedgerConfig era)
    , shelleyTriggerHardFork :: !TriggerHardFork
    }
  deriving (Generic, NoThunks)

instance ShelleyBasedEra era => HasPartialLedgerConfig (ShelleyBlock era) where
  type PartialLedgerConfig (ShelleyBlock era) = ShelleyPartialLedgerConfig era

  -- Replace the dummy 'EpochInfo' with the real one
  completeLedgerConfig _ epochInfo (ShelleyPartialLedgerConfig cfg _) =
      cfg {
          shelleyLedgerGlobals = (shelleyLedgerGlobals cfg) {
              SL.epochInfo = epochInfo
            }
        }
