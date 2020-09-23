{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.ThreadNet.Infra.ShelleyShelley (
  -- * Config
  ShelleyShelleyConsensusConfig,
  pattern ShelleyShelleyConsensusConfig,
  ShelleyShelleyLedgerConfig,
  pattern ShelleyShelleyLedgerConfig,
  -- * Blocks
  ShelleyShelleyEras,
  ShelleyShelleyBlock,
  pattern BlockShelley1,
  pattern BlockShelley2,
  -- * Transactions
  pattern GenTxShelley1,
  pattern GenTxShelley2,
  -- * Node
  protocolInfoShelleyShelley,
  -- * Miscellany
  epochSizeFromPartial,
  ) where

import           Control.Exception (assert)
import           Data.Functor.Contravariant (contramap)
import qualified Data.Map as Map
import           Data.SOP.Strict (I (..), NP (..), NS (..))

-- Upstream libraries

import           Cardano.Slotting.EpochInfo (fixedSizeEpochInfo)

import qualified Shelley.Spec.Ledger.BaseTypes as SL

-- Consensus

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Basics (applyChainTick)
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (protocolLedgerView)
import           Ouroboros.Consensus.Node
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Storage.ImmutableDB (simpleChunkInfo)
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util.Assert (assertWithMsg)
import           Ouroboros.Consensus.Util.Counting (exactlyHead, exactlyTwo)
import           Ouroboros.Consensus.Util.IOLike (IOLike)

-- HardFork

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation
import qualified Ouroboros.Consensus.HardFork.Combinator.State.Types as HFC
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.InPairs as InPairs
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Tails as Tails
import qualified Ouroboros.Consensus.HardFork.History as History

-- Shelley

import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Node
import           Ouroboros.Consensus.Shelley.Protocol
import           Test.ThreadNet.Infra.Shelley

-- Shelley via Cardano

import           Ouroboros.Consensus.Cardano.Block (ShelleyEra)
import           Ouroboros.Consensus.Cardano.Node (TriggerHardFork (..),
                     mkPartialLedgerConfigShelley)

{-------------------------------------------------------------------------------
  Block type
-------------------------------------------------------------------------------}

-- | Two eras, both Shelley and 100% compatible.
type ShelleyShelleyEras c =
    '[ShelleyBlock (ShelleyEra c), ShelleyBlock (ShelleyEra c)]

type ShelleyShelleyBlock c = HardForkBlock (ShelleyShelleyEras c)

{-------------------------------------------------------------------------------
  Pattern synonyms, for encapsulation and legibility
-------------------------------------------------------------------------------}

pattern BlockShelley1 :: ShelleyBlock (ShelleyEra c) -> ShelleyShelleyBlock c
pattern BlockShelley1 b = HardForkBlock (OneEraBlock (Z (I b)))

pattern BlockShelley2 :: ShelleyBlock (ShelleyEra c) -> ShelleyShelleyBlock c
pattern BlockShelley2 b = HardForkBlock (OneEraBlock (S (Z (I b))))

{-# COMPLETE BlockShelley1, BlockShelley2 #-}

type ShelleyShelleyHeader c = Header (ShelleyShelleyBlock c)

pattern HeaderShelley1 ::
    Header (ShelleyBlock (ShelleyEra c)) -> ShelleyShelleyHeader c
pattern HeaderShelley1 h = HardForkHeader (OneEraHeader (Z h))

pattern HeaderShelley2 ::
    Header (ShelleyBlock (ShelleyEra c)) -> ShelleyShelleyHeader c
pattern HeaderShelley2 h = HardForkHeader (OneEraHeader (S (Z h)))

{-# COMPLETE HeaderShelley1, HeaderShelley2 #-}

type ShelleyShelleyGenTx c = GenTx (ShelleyShelleyBlock c)

pattern GenTxShelley1 ::
    GenTx (ShelleyBlock (ShelleyEra c)) -> ShelleyShelleyGenTx c
pattern GenTxShelley1 tx = HardForkGenTx (OneEraGenTx (Z tx))

pattern GenTxShelley2 ::
    GenTx (ShelleyBlock (ShelleyEra c)) -> ShelleyShelleyGenTx c
pattern GenTxShelley2 tx = HardForkGenTx (OneEraGenTx (S (Z tx)))

{-# COMPLETE GenTxShelley1, GenTxShelley2 #-}

type ShelleyShelleyConsensusConfig c =
  ConsensusConfig (HardForkProtocol (ShelleyShelleyEras c))

pattern ShelleyShelleyConsensusConfig
  :: PartialConsensusConfig (BlockProtocol (ShelleyBlock (ShelleyEra c)))
  -> PartialConsensusConfig (BlockProtocol (ShelleyBlock (ShelleyEra c)))
  -> ShelleyShelleyConsensusConfig c
pattern ShelleyShelleyConsensusConfig cfgShelley1 cfgShelley2 <-
    HardForkConsensusConfig {
        hardForkConsensusConfigPerEra = PerEraConsensusConfig
          (   WrapPartialConsensusConfig cfgShelley1
           :* WrapPartialConsensusConfig cfgShelley2
           :* Nil
          )
      }

{-# COMPLETE ShelleyShelleyConsensusConfig #-}

type ShelleyShelleyBlockConfig c = BlockConfig (ShelleyShelleyBlock c)

pattern ShelleyShelleyBlockConfig
  :: BlockConfig (ShelleyBlock (ShelleyEra c))
  -> BlockConfig (ShelleyBlock (ShelleyEra c))
  -> ShelleyShelleyBlockConfig c
pattern ShelleyShelleyBlockConfig cfgShelley1 cfgShelley2 =
    HardForkBlockConfig (PerEraBlockConfig (cfgShelley1 :* cfgShelley2 :* Nil))

{-# COMPLETE ShelleyShelleyBlockConfig #-}

type ShelleyShelleyCodecConfig c = CodecConfig (ShelleyShelleyBlock c)

pattern ShelleyShelleyCodecConfig
  :: CodecConfig (ShelleyBlock (ShelleyEra c))
  -> CodecConfig (ShelleyBlock (ShelleyEra c))
  -> ShelleyShelleyCodecConfig c
pattern ShelleyShelleyCodecConfig cfgShelley1 cfgShelley2 =
    HardForkCodecConfig (PerEraCodecConfig (cfgShelley1 :* cfgShelley2 :* Nil))

{-# COMPLETE ShelleyShelleyCodecConfig #-}

type ShelleyShelleyLedgerConfig c =
    HardForkLedgerConfig (ShelleyShelleyEras c)

pattern ShelleyShelleyLedgerConfig
  :: PartialLedgerConfig (ShelleyBlock (ShelleyEra c))
  -> PartialLedgerConfig (ShelleyBlock (ShelleyEra c))
  -> ShelleyShelleyLedgerConfig c
pattern ShelleyShelleyLedgerConfig cfgShelley1 cfgShelley2 <-
    HardForkLedgerConfig {
        hardForkLedgerConfigPerEra = PerEraLedgerConfig
          (   WrapPartialLedgerConfig cfgShelley1
           :* WrapPartialLedgerConfig cfgShelley2
           :* Nil
          )
      }

{-# COMPLETE ShelleyShelleyLedgerConfig #-}

pattern ShelleyShelleyNodeToNodeVersion2 ::
    BlockNodeToNodeVersion (ShelleyShelleyBlock c)
pattern ShelleyShelleyNodeToNodeVersion2 =
    HardForkNodeToNodeEnabled (
         EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* Nil
      )

pattern ShelleyShelleyNodeToClientVersion2 ::
    BlockNodeToClientVersion (ShelleyShelleyBlock c)
pattern ShelleyShelleyNodeToClientVersion2 =
    HardForkNodeToClientEnabled (
         EraNodeToClientEnabled ShelleyNodeToClientVersion1
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion1
      :* Nil
      )

{-------------------------------------------------------------------------------
  Consensus instances
-------------------------------------------------------------------------------}

instance TPraosCrypto (ShelleyEra c) => SerialiseHFC (ShelleyShelleyEras c)
   -- use defaults

instance TPraosCrypto (ShelleyEra c) => CanHardFork (ShelleyShelleyEras c) where
  hardForkEraTranslation = EraTranslation {
        translateLedgerState   = PCons idTranslation PNil
      , translateChainDepState = PCons idTranslation PNil
      , translateLedgerView    =
          (\x -> PCons x PNil) $
          InPairs.RequireBoth $ \(WrapLedgerConfig cfg1) _cfg2 ->
          HFC.TranslateForecast $ \_bound forecastFor st ->
          -- TODO check forecastFor against stability window... of both eras?
          return $ WrapTickedLedgerView $ protocolLedgerView cfg1 $ applyChainTick cfg1 forecastFor st
      }
    where
      idTranslation :: forall inn out a.
        InPairs.RequiringBoth inn (HFC.Translate out) a a
      idTranslation =
          InPairs.RequireBoth $ \_ _ ->
          HFC.Translate $ \_epochNo ->
          id
  hardForkChainSel  = Tails.mk2 CompareBlockNo
  hardForkInjectTxs = InPairs.mk2 $ InPairs.ignoringBoth cannotInjectTx

instance TPraosCrypto (ShelleyEra c) => RunNode (ShelleyShelleyBlock c) where
  -- TODO need to add 2 like Cardano?
  nodeBlockFetchSize = \case
      HeaderShelley1 h -> nodeBlockFetchSize h
      HeaderShelley2 h -> nodeBlockFetchSize h

  nodeImmutableDbChunkInfo =
        simpleChunkInfo
      . History.eraEpochSize
      . exactlyHead
      . History.getShape
      . hardForkLedgerConfigShape
      . configLedger

  nodeInitChainDB cfg initChainDB =
      nodeInitChainDB
        shelley1Cfg
        (contramap BlockShelley1 initChainDB)
    where
      TopLevelConfig {
          topLevelConfigProtocol
        , topLevelConfigLedger
        , topLevelConfigBlock
        , topLevelConfigCodec
        } = cfg

      ShelleyShelleyConsensusConfig partialCfg1 _    = topLevelConfigProtocol
      ShelleyShelleyLedgerConfig shelley1LedgerCfg _ = topLevelConfigLedger
      ShelleyShelleyBlockConfig shelley1BlockCfg _   = topLevelConfigBlock
      ShelleyShelleyCodecConfig shelley1CodecCfg _   = topLevelConfigCodec

      shelley1Cfg :: TopLevelConfig (ShelleyBlock (ShelleyEra c))
      shelley1Cfg = TopLevelConfig {
          topLevelConfigProtocol =
            completeConsensusConfig
              (Proxy :: Proxy (BlockProtocol (ShelleyBlock (ShelleyEra c))))
              epochInfo
              partialCfg1
        , topLevelConfigLedger   =
            completeLedgerConfig
              (Proxy :: Proxy (ShelleyBlock (ShelleyEra c)))
              epochInfo
              shelley1LedgerCfg
        , topLevelConfigBlock    = shelley1BlockCfg
        , topLevelConfigCodec    = shelley1CodecCfg
        }

      -- Suitable only for this narrow context
      epochInfo :: EpochInfo Identity
      epochInfo = fixedSizeEpochInfo (epochSizeFromPartial partialCfg1)

  nodeCheckIntegrity cfg = \case
      BlockShelley1 blk ->
        verifyBlockIntegrity (tpraosSlotsPerKESPeriod partialCfg1) blk
      BlockShelley2 blk ->
        verifyBlockIntegrity (tpraosSlotsPerKESPeriod partialCfg2) blk
    where
      TopLevelConfig {topLevelConfigProtocol} = cfg

      ShelleyShelleyConsensusConfig partialCfg1 partialCfg2 =
          topLevelConfigProtocol

instance
     TPraosCrypto (ShelleyEra c)
  => SupportedNetworkProtocolVersion (ShelleyShelleyBlock c) where
  supportedNodeToNodeVersions _ = Map.fromList $
      [ (NodeToNodeV_2, ShelleyShelleyNodeToNodeVersion2)
      , (NodeToNodeV_3, ShelleyShelleyNodeToNodeVersion2)
      ]

  supportedNodeToClientVersions _ = Map.fromList $
      [ (NodeToClientV_3, ShelleyShelleyNodeToClientVersion2)
      ]

{-------------------------------------------------------------------------------
  Protocol info
-------------------------------------------------------------------------------}

protocolInfoShelleyShelley
  :: forall c m. (IOLike m, TPraosCrypto (ShelleyEra c))
     -- Shelley1
  => ShelleyGenesis (ShelleyEra c)
  -> Nonce
  -> ProtVer
  -> MaxMajorProtVer
  -> CoreNode (ShelleyEra c)
     -- Shelley2
  -> ShelleyGenesis (ShelleyEra c)
  -> Nonce
     -- ^ The initial nonce for the Shelley era, typically derived from the
     -- hash of Shelley Genesis config JSON file.
  -> ProtVer
  -> MaxMajorProtVer
  -> CoreNode (ShelleyEra c)
     -- Hard fork
  -> TriggerHardFork -- ^ Transition from Shelley1 to Shelley2
  -> TriggerHardFork -- ^ Transition from Shelley2 to ShelleyMA
  -> ProtocolInfo m (ShelleyShelleyBlock c)
protocolInfoShelleyShelley
  genesis1 initialNonce1 protVer1 maxMajorPV1 coreNode1
  genesis2 initialNonce2 protVer2 maxMajorPV2 coreNode2
  shelley1TriggerHardFork shelley2TriggerHardFork =
    assertWithMsg (validateGenesis genesis1) $
    assertWithMsg (validateGenesis genesis2) $
    ProtocolInfo {
        pInfoConfig = cfg
      , pInfoInitLedger = ExtLedgerState {
            ledgerState =
              HardForkLedgerState $
                initHardForkState initLedgerState1
          , headerState =
              genesisHeaderState $
                initHardForkState $
                  WrapChainDepState $
                    headerStateChainDep initHeaderState1
          }
      , pInfoBlockForging = blockForging
      }
  where
    creds1 = mkLeaderCredentials coreNode1
    creds2 = mkLeaderCredentials coreNode2

    -- Shelley1
    ProtocolInfo {
        pInfoInitLedger = ExtLedgerState {
            ledgerState = initLedgerState1
          , headerState = initHeaderState1
          }
      } = mkProtocolRealTPraos @m genesis1 initialNonce1 protVer1 coreNode1

    blockConfig1 :: BlockConfig (ShelleyBlock (ShelleyEra c))
    blockConfig1 =
        mkShelleyBlockConfig
          protVer1
          genesis1
          [tpraosBlockIssuerVKey creds1]

    partialConsensusConfig1 ::
         PartialConsensusConfig (BlockProtocol (ShelleyBlock (ShelleyEra c)))
    partialConsensusConfig1 = tpraosParams

    partialLedgerConfig1 :: PartialLedgerConfig (ShelleyBlock (ShelleyEra c))
    partialLedgerConfig1 =
        mkPartialLedgerConfigShelley
          genesis1
          maxMajorPV1
          shelley1TriggerHardFork

    k1 :: SecurityParam
    k1 = SecurityParam $ sgSecurityParam genesis1

    -- Shelley2

    tpraosParams :: TPraosParams
    tpraosParams =
        mkTPraosParams
          maxMajorPV2
          initialNonce2
          genesis2

    blockConfig2 :: BlockConfig (ShelleyBlock (ShelleyEra c))
    blockConfig2 =
        mkShelleyBlockConfig
          protVer2
          genesis2
          [tpraosBlockIssuerVKey creds2]

    partialConsensusConfig2 ::
        PartialConsensusConfig (BlockProtocol (ShelleyBlock (ShelleyEra c)))
    partialConsensusConfig2 = tpraosParams

    partialLedgerConfig2 ::
        PartialLedgerConfig (ShelleyBlock (ShelleyEra c))
    partialLedgerConfig2 =
        mkPartialLedgerConfigShelley
          genesis2
          maxMajorPV2
          shelley2TriggerHardFork

    k2 :: SecurityParam
    k2 = SecurityParam $ sgSecurityParam genesis2

    -- ShelleyShelley

    k :: SecurityParam
    k = assert (k1 == k2) k1

    shape :: History.Shape (ShelleyShelleyEras c)
    shape = History.Shape $
      exactlyTwo
        (shelleyEraParams History.NoLowerBound genesis1)
        (shelleyEraParams History.NoLowerBound genesis2)

    cfg :: TopLevelConfig (ShelleyShelleyBlock c)
    cfg = TopLevelConfig {
        topLevelConfigProtocol = HardForkConsensusConfig {
            hardForkConsensusConfigK      = k
          , hardForkConsensusConfigShape  = shape
          , hardForkConsensusConfigPerEra = PerEraConsensusConfig
              (  WrapPartialConsensusConfig partialConsensusConfig1
              :* WrapPartialConsensusConfig partialConsensusConfig2
              :* Nil
              )
          }
      , topLevelConfigLedger = HardForkLedgerConfig {
            hardForkLedgerConfigShape  = shape
          , hardForkLedgerConfigPerEra = PerEraLedgerConfig
              (  WrapPartialLedgerConfig partialLedgerConfig1
              :* WrapPartialLedgerConfig partialLedgerConfig2
              :* Nil
              )
          }
      , topLevelConfigBlock =
          ShelleyShelleyBlockConfig
            blockConfig1
            blockConfig2
      , topLevelConfigCodec =
          ShelleyShelleyCodecConfig
            ShelleyCodecConfig
            ShelleyCodecConfig
      }

    blockForging :: [m (BlockForging m (ShelleyShelleyBlock c))]
    blockForging = [
        fmap
          (hardForkBlockForging . Z)
          (shelleyBlockForging tpraosParams creds1)
      , fmap
          (hardForkBlockForging . S . Z)
          (shelleyBlockForging tpraosParams creds2)
      ]

{-------------------------------------------------------------------------------
  Miscellany
-------------------------------------------------------------------------------}

epochSizeFromPartial ::
       PartialConsensusConfig (BlockProtocol (ShelleyBlock (ShelleyEra c)))
    -> EpochSize
epochSizeFromPartial pcfg =
    mkEpochSize (tpraosSecurityParam pcfg) f
  where
    f :: Rational
    f =
        (SL.unitIntervalToRational . SL.activeSlotVal) $
        tpraosLeaderF pcfg
