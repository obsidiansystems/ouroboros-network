{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Voltaire.Prototype.Node (
    VoltairePrototypeHardForkConstraints
  , MaxMajorProtVer (..)
  , ProtocolParamsVoltairePrototype (..)
  , ProtocolParamsTransition (..)
  , TriggerHardFork (..)
  , protocolClientInfoVoltairePrototype
  , protocolInfoVoltairePrototype
    -- * SupportedNetworkProtocolVersion
  , pattern VoltairePrototypeNodeToClientVersion1
  , pattern VoltairePrototypeNodeToClientVersion2
  , pattern VoltairePrototypeNodeToNodeVersion1
  , pattern VoltairePrototypeNodeToNodeVersion2
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import qualified Data.ByteString.Short as Short
import qualified Data.Map.Strict as Map
import           Data.SOP.Strict hiding (shape, shift)
import           Data.Word (Word16)

import           Cardano.Binary (DecoderError (..), enforceSize)
import           Cardano.Prelude (cborError)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Storage.Serialisation
import           Ouroboros.Consensus.Util.Assert
import           Ouroboros.Consensus.Util.Counting
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.OptNP (OptNP (..))
import qualified Ouroboros.Consensus.Util.OptNP as OptNP

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Embed.Nary
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation

import qualified Cardano.Ledger.Era as SL
import           Cardano.Ledger.Val (coin, (<->))
import qualified Cardano.Ledger.Val as Val
import           Ouroboros.Consensus.Voltaire.Prototype.ShelleyBased
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger as Shelley
import           Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion
import           Ouroboros.Consensus.Shelley.Node
import           Ouroboros.Consensus.Shelley.Protocol (TPraosParams (..))
import qualified Ouroboros.Consensus.Shelley.Protocol as Shelley
import           Ouroboros.Consensus.Shelley.ShelleyBased
import qualified Shelley.Spec.Ledger.API as SL

import           Ouroboros.Consensus.Voltaire.Prototype.Block
import           Ouroboros.Consensus.Voltaire.Prototype.CanHardFork

{-------------------------------------------------------------------------------
  SerialiseHFC
-------------------------------------------------------------------------------}

instance ( VoltairePrototypeHardForkConstraints 'VoltairePrototype_One c
         , VoltairePrototypeHardForkConstraints 'VoltairePrototype_Two c
         )
  => SerialiseHFC (VoltairePrototypeEras c) where
  encodeDiskHfcBlock (VoltairePrototypeCodecConfig ccfgShelley ccfgVoltairePrototypeOne ccfgVoltairePrototypeTwo) = \case
      -- For Shelley and later eras, we need to prepend the hard fork envelope.
      BlockShelley blockShelley -> prependTag 2 $ encodeDisk ccfgShelley blockShelley
      BlockVoltairePrototypeOne blockVoltairePrototype -> prependTag 99 $ encodeDisk ccfgVoltairePrototypeOne blockVoltairePrototype
      BlockVoltairePrototypeTwo blockVoltairePrototype -> prependTag 100 $ encodeDisk ccfgVoltairePrototypeTwo blockVoltairePrototype
  decodeDiskHfcBlock (VoltairePrototypeCodecConfig ccfgShelley ccfgVoltairePrototypeOne ccfgVoltairePrototypeTwo) = do
      enforceSize "VoltairePrototypeBlock" 2
      CBOR.decodeWord >>= \case
        -- We don't have to drop the first two bytes from the 'ByteString'
        -- passed to the decoder as slicing already takes care of this.
        2  -> fmap BlockShelley <$> decodeDisk ccfgShelley
        99 -> fmap BlockVoltairePrototypeOne <$> decodeDisk ccfgVoltairePrototypeOne
        100 -> fmap BlockVoltairePrototypeTwo <$> decodeDisk ccfgVoltairePrototypeTwo
        t  -> cborError $ DecoderErrorUnknownTag "VoltairePrototypeBlock" (fromIntegral t)

  reconstructHfcPrefixLen _ = PrefixLen 2

  reconstructHfcNestedCtxt _ prefix _ =
      case Short.index prefix 1 of
        2  -> SomeSecond $ NestedCtxt (NCZ Shelley.CtxtShelley)
        99 -> SomeSecond $ NestedCtxt (NCS (NCZ Shelley.CtxtShelley))
        100 -> SomeSecond $ NestedCtxt (NCS (NCS (NCZ Shelley.CtxtShelley)))
        _  -> error $ "VoltairePrototypeBlock: invalid prefix " <> show prefix

  getHfcBinaryBlockInfo = \case
      -- For Shelley and the later eras, we need to account for the two extra
      -- bytes of the envelope.
      BlockShelley blockShelley ->
        shiftHeaderOffset 2 $ getBinaryBlockInfo blockShelley
      BlockVoltairePrototypeOne blockVoltairePrototype ->
        shiftHeaderOffset 2 $ getBinaryBlockInfo blockVoltairePrototype
      BlockVoltairePrototypeTwo blockVoltairePrototype ->
        shiftHeaderOffset 2 $ getBinaryBlockInfo blockVoltairePrototype
    where
      shiftHeaderOffset :: Word16 -> BinaryBlockInfo -> BinaryBlockInfo
      shiftHeaderOffset shift binfo = binfo {
            headerOffset = headerOffset binfo + shift
          }

  estimateHfcBlockSize = \case
      -- For Shelley and later eras, we add two extra bytes, see the
      -- 'SerialiseHFC' instance.
      HeaderShelley headerShelley -> estimateBlockSize headerShelley + 2
      HeaderVoltairePrototypeOne headerVoltairePrototype -> estimateBlockSize headerVoltairePrototype + 2
      HeaderVoltairePrototypeTwo headerVoltairePrototype -> estimateBlockSize headerVoltairePrototype + 2

-- | Prepend the given tag by creating a CBOR 2-tuple with the tag as the
-- first element and the given 'Encoding' as the second.
prependTag :: Word -> Encoding -> Encoding
prependTag tag payload = mconcat [
      CBOR.encodeListLen 2
    , CBOR.encodeWord tag
    , payload
    ]

{-------------------------------------------------------------------------------
  SupportedNetworkProtocolVersion instance
-------------------------------------------------------------------------------}

-- | The hard fork enabled with the Shelley and VoltairePrototype eras enabled.
pattern VoltairePrototypeNodeToNodeVersion1 :: BlockNodeToNodeVersion (VoltairePrototypeBlock c)
pattern VoltairePrototypeNodeToNodeVersion1 =
    HardForkNodeToNodeEnabled
      HardForkSpecificNodeToNodeVersion1
      (  EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* EraNodeToNodeDisabled
      :* Nil
      )

-- | The hard fork enabled with the Shelley and VoltairePrototype eras enabled.
pattern VoltairePrototypeNodeToNodeVersion2 :: BlockNodeToNodeVersion (VoltairePrototypeBlock c)
pattern VoltairePrototypeNodeToNodeVersion2 =
    HardForkNodeToNodeEnabled
      HardForkSpecificNodeToNodeVersion1
      (  EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* Nil
      )

-- | The hard fork enabled, and the Shelley era enabled using 'ShelleyNodeToClientVersion3' protocol.
pattern VoltairePrototypeNodeToClientVersion1 :: BlockNodeToClientVersion (VoltairePrototypeBlock c)
pattern VoltairePrototypeNodeToClientVersion1 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion2
      (  EraNodeToClientEnabled ShelleyNodeToClientVersion3
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion3
      :* EraNodeToClientDisabled
      :* Nil
      )

-- | The hard fork enabled, and the Shelley and VoltairePrototype eras enabled using 'ShelleyNodeToClientVersion3' protocol.
pattern VoltairePrototypeNodeToClientVersion2 :: BlockNodeToClientVersion (VoltairePrototypeBlock c)
pattern VoltairePrototypeNodeToClientVersion2 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion2
      (  EraNodeToClientEnabled ShelleyNodeToClientVersion3
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion3
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion3
      :* Nil
      )

instance ( VoltairePrototypeHardForkConstraints 'VoltairePrototype_One c
         , VoltairePrototypeHardForkConstraints 'VoltairePrototype_Two c
         )
        => SupportedNetworkProtocolVersion (VoltairePrototypeBlock c) where
  supportedNodeToNodeVersions _ = Map.fromList $
      [ (NodeToNodeV_3, VoltairePrototypeNodeToNodeVersion1)
      , (NodeToNodeV_3, VoltairePrototypeNodeToNodeVersion2)
      ]

  supportedNodeToClientVersions _ = Map.fromList $
      [ (NodeToClientV_4, VoltairePrototypeNodeToClientVersion1)
      , (NodeToClientV_4, VoltairePrototypeNodeToClientVersion2)
      ]

  latestReleasedNodeVersion = latestReleasedNodeVersionDefault

{-------------------------------------------------------------------------------
  ProtocolInfo
-------------------------------------------------------------------------------}

-- | Parameters needed to transition between two eras.
--
-- The two eras are phantom type parameters of this type to avoid mixing up
-- multiple 'ProtocolParamsTransition's
data ProtocolParamsTransition eraFrom eraTo = ProtocolParamsTransition {
      transitionTrigger    :: TriggerHardFork
    }

-- | Parameters needed to run Shelley
data ProtocolParamsVoltairePrototype = ProtocolParamsVoltairePrototype {
      exampleProtVer :: SL.ProtVer
    }

-- | Create a 'ProtocolInfo' for 'VoltairePrototypeBlock'
--
-- NOTE: the initial staking and funds in the 'ShelleyGenesis' are ignored,
-- /unless/ configured to skip the Shelley era and hard fork to VoltairePrototype
-- era from the start using @TriggerHardForkAtEpoch 0@ for testing purposes.
--
-- PRECONDITION: only a single set of Shelley credentials is allowed when used
-- for mainnet (check against @'SL.gNetworkId' 'shelleyBasedGenesis'@).
protocolInfoVoltairePrototype ::
     forall c m.
     ( IOLike m
     , VoltairePrototypeHardForkConstraints 'VoltairePrototype_One c
     , VoltairePrototypeHardForkConstraints 'VoltairePrototype_Two c
     )
  => ProtocolParamsShelleyBased (ShelleyEra c)
  -> ProtocolParamsShelley
  -> ProtocolParamsVoltairePrototype
  -> ProtocolParamsTransition
       (ShelleyBlock (ShelleyEra c))
       (ShelleyBlock (VoltairePrototypeEra 'VoltairePrototype_One c))
  -> ProtocolParamsTransition
       (ShelleyBlock (VoltairePrototypeEra 'VoltairePrototype_One c))
       (ShelleyBlock (VoltairePrototypeEra 'VoltairePrototype_Two c))
  -> ProtocolInfo m (VoltairePrototypeBlock c)
protocolInfoVoltairePrototype ProtocolParamsShelleyBased {
                        shelleyBasedGenesis           = genesisShelley
                      , shelleyBasedInitialNonce      = initialNonceShelley
                      , shelleyBasedLeaderCredentials = credssShelleyBased
                      }
                    ProtocolParamsShelley {
                        shelleyProtVer = protVerShelley
                      }
                    ProtocolParamsVoltairePrototype {
                        exampleProtVer = protVerVoltairePrototype
                      }
                    ProtocolParamsTransition {
                        transitionTrigger = triggerHardForkShelleyVoltairePrototypeOne
                      }
                    ProtocolParamsTransition {
                        transitionTrigger = triggerHardForkPrototypeOnePrototypeTwo
                      }
  | SL.Mainnet <- SL.sgNetworkId genesisShelley
  , length credssShelleyBased > 1
  = error "Multiple Shelley-based credentials not allowed for mainnet"
  | otherwise
  = assertWithMsg (validateGenesis genesisShelley) $
    ProtocolInfo {
        pInfoConfig       = cfg
      , pInfoInitLedger   = initExtLedgerStateVoltairePrototype
      , pInfoBlockForging = blockForging
      }
  where
    -- The major protocol version of the last era is the maximum major protocol
    -- version we support.
    maxMajorProtVer :: MaxMajorProtVer
    maxMajorProtVer = MaxMajorProtVer (pvMajor protVerVoltairePrototype)

    -- Shelley

    tpraosParams :: TPraosParams
    tpraosParams@TPraosParams { tpraosSlotsPerKESPeriod } =
        Shelley.mkTPraosParams
          maxMajorProtVer
          initialNonceShelley
          genesisShelley

    blockConfigShelley :: BlockConfig (ShelleyBlock (ShelleyEra c))
    blockConfigShelley =
        Shelley.mkShelleyBlockConfig
          protVerShelley
          genesisShelley
          (tpraosBlockIssuerVKey <$> credssShelleyBased)

    partialConsensusConfigShelley ::
         PartialConsensusConfig (BlockProtocol (ShelleyBlock (ShelleyEra c)))
    partialConsensusConfigShelley = tpraosParams

    partialLedgerConfigShelley :: PartialLedgerConfig (ShelleyBlock (ShelleyEra c))
    partialLedgerConfigShelley =
        mkPartialLedgerConfigShelley
          genesisShelley
          maxMajorProtVer
          triggerHardForkShelleyVoltairePrototypeOne

    kShelley :: SecurityParam
    kShelley = SecurityParam $ sgSecurityParam genesisShelley

    -- Allegra

    genesisVoltairePrototypeOne :: ShelleyGenesis (VoltairePrototypeEra 'VoltairePrototype_One c)
    genesisVoltairePrototypeOne = SL.translateEra' () genesisShelley

    genesisVoltairePrototypeTwo  :: ShelleyGenesis (VoltairePrototypeEra 'VoltairePrototype_Two c)
    genesisVoltairePrototypeTwo = SL.translateEra' () genesisVoltairePrototypeOne

    blockConfigVoltairePrototypeOne :: BlockConfig (ShelleyBlock (VoltairePrototypeEra 'VoltairePrototype_One c))
    blockConfigVoltairePrototypeOne =
        Shelley.mkShelleyBlockConfig
          protVerVoltairePrototype
          genesisVoltairePrototypeOne
          (tpraosBlockIssuerVKey <$> credssShelleyBased)

    blockConfigVoltairePrototypeTwo :: BlockConfig (ShelleyBlock (VoltairePrototypeEra 'VoltairePrototype_Two c))
    blockConfigVoltairePrototypeTwo =
        Shelley.mkShelleyBlockConfig
          protVerVoltairePrototype
          genesisVoltairePrototypeTwo
          (tpraosBlockIssuerVKey <$> credssShelleyBased)

    partialConsensusConfigVoltairePrototype ::
         PartialConsensusConfig (BlockProtocol (ShelleyBlock (VoltairePrototypeEra 'VoltairePrototype_One c)))
    partialConsensusConfigVoltairePrototype = tpraosParams

    partialLedgerConfigVoltairePrototypeOne :: PartialLedgerConfig (ShelleyBlock (VoltairePrototypeEra 'VoltairePrototype_One c))
    partialLedgerConfigVoltairePrototypeOne =
        mkPartialLedgerConfigShelley
          genesisVoltairePrototypeOne
          maxMajorProtVer
          triggerHardForkPrototypeOnePrototypeTwo

    partialLedgerConfigVoltairePrototypeTwo :: PartialLedgerConfig (ShelleyBlock (VoltairePrototypeEra 'VoltairePrototype_Two c))
    partialLedgerConfigVoltairePrototypeTwo =
        mkPartialLedgerConfigShelley
          genesisVoltairePrototypeTwo
          maxMajorProtVer
          TriggerHardForkNever
    --
    -- Cardano

    k :: SecurityParam
    k = kShelley

    shape :: History.Shape (VoltairePrototypeEras c)
    shape = History.Shape $ Exactly $
           K (Shelley.shelleyEraParams genesisShelley)
        :* K (Shelley.shelleyEraParams genesisVoltairePrototypeOne)
        :* K (Shelley.shelleyEraParams genesisVoltairePrototypeTwo)
        :* Nil

    cfg :: TopLevelConfig (VoltairePrototypeBlock c)
    cfg = TopLevelConfig {
        topLevelConfigProtocol = HardForkConsensusConfig {
            hardForkConsensusConfigK      = k
          , hardForkConsensusConfigShape  = shape
          , hardForkConsensusConfigPerEra = PerEraConsensusConfig
              (  WrapPartialConsensusConfig partialConsensusConfigShelley
              :* WrapPartialConsensusConfig partialConsensusConfigVoltairePrototype
              :* WrapPartialConsensusConfig partialConsensusConfigVoltairePrototype
              :* Nil
              )
          }
      , topLevelConfigLedger = HardForkLedgerConfig {
            hardForkLedgerConfigShape  = shape
          , hardForkLedgerConfigPerEra = PerEraLedgerConfig
              (  WrapPartialLedgerConfig partialLedgerConfigShelley
              :* WrapPartialLedgerConfig partialLedgerConfigVoltairePrototypeOne
              :* WrapPartialLedgerConfig partialLedgerConfigVoltairePrototypeTwo
              :* Nil
              )
          }
      , topLevelConfigBlock =
          VoltairePrototypeBlockConfig
            blockConfigShelley
            blockConfigVoltairePrototypeOne
            blockConfigVoltairePrototypeTwo
      , topLevelConfigCodec =
          VoltairePrototypeCodecConfig
            Shelley.ShelleyCodecConfig
            Shelley.ShelleyCodecConfig
            Shelley.ShelleyCodecConfig
      , topLevelConfigStorage =
          VoltairePrototypeStorageConfig
            (Shelley.ShelleyStorageConfig tpraosSlotsPerKESPeriod k)
            (Shelley.ShelleyStorageConfig tpraosSlotsPerKESPeriod k)
            (Shelley.ShelleyStorageConfig tpraosSlotsPerKESPeriod k)
      }

    -- Register the initial staking and initial funds (if provided in the genesis config) in
    -- the ledger state.
    initExtLedgerStateVoltairePrototype :: ExtLedgerState (VoltairePrototypeBlock c)
    initExtLedgerStateVoltairePrototype = ExtLedgerState {
          headerState = initHeaderState
        , ledgerState = overShelleyBasedLedgerState register initLedgerState
        }
      where
        initHeaderState :: HeaderState (VoltairePrototypeBlock c)
        initLedgerState :: LedgerState (VoltairePrototypeBlock c)
        ExtLedgerState initLedgerState initHeaderState =
          injectInitialExtLedgerState cfg $ ExtLedgerState {
            ledgerState = Shelley.ShelleyLedgerState {
              Shelley.shelleyLedgerTip        = Origin
            , Shelley.shelleyLedgerState      = SL.chainNes initShelleyState
            , Shelley.shelleyLedgerTransition = Shelley.ShelleyTransitionInfo {Shelley.shelleyAfterVoting = 0}
            }
          , headerState = genesisHeaderState initChainDepState
          }

        initChainDepState :: Shelley.TPraosState c
        initChainDepState = Shelley.TPraosState Origin $
          SL.ChainDepState {
              SL.csProtocol = SL.PrtclState
                (SL.chainOCertIssue     initShelleyState)
                (SL.chainEvolvingNonce  initShelleyState)
                (SL.chainCandidateNonce initShelleyState)
            , SL.csTickn = SL.TicknState
                (SL.chainEpochNonce     initShelleyState)
                (SL.chainPrevEpochNonce initShelleyState)
            , SL.csLabNonce =
                (SL.chainPrevEpochNonce initShelleyState)
            }

        initShelleyState :: SL.ChainState (ShelleyEra c)
        initShelleyState =
            overNewEpochState
              (registerGenesisStaking (SL.sgStaking genesisShelley)) $
              SL.initialShelleyState
                Origin
                0
                (SL.genesisUTxO genesisShelley)
                (coin $ Val.inject (SL.word64ToCoin (SL.sgMaxLovelaceSupply genesisShelley))
                    <-> SL.balance (SL.genesisUTxO genesisShelley))
                (SL.sgGenDelegs genesisShelley)
                (SL.sgProtocolParams genesisShelley)
                initialNonceShelley

        overNewEpochState ::
             (SL.NewEpochState era -> SL.NewEpochState era)
          -> (SL.ChainState    era -> SL.ChainState    era)
        overNewEpochState f cs = cs { SL.chainNes = f (SL.chainNes cs) }

        register ::
             (EraCrypto era ~ c, ShelleyBasedEra era)
          => LedgerState (ShelleyBlock era)
          -> LedgerState (ShelleyBlock era)
        register st = st {
              Shelley.shelleyLedgerState =
                -- We must first register the initial funds, because the stake
                -- information depends on it.
                  registerGenesisStaking
                    (SL.sgStaking genesisShelley)
                . registerInitialFunds
                    (SL.sgInitialFunds genesisShelley)
                $ Shelley.shelleyLedgerState st
            }

    -- | For each element in the list, a block forging thread will be started.
    --
    -- When no credentials are passed, there will be no threads.
    --
    -- Typically, there will only be a single set of credentials for Shelley.
    --
    -- In case there are multiple credentials for Shelley, which is only done
    -- for testing/benchmarking purposes, we'll have a separate thread for each
    -- of them.
    blockForging :: m [BlockForging m (VoltairePrototypeBlock c)]
    blockForging = do
        shelleyBased :: [ OptNP 'False (BlockForging m) (VoltairePrototypeEras c) ] <- blockForgingShelleyBased
        return $ hardForkBlockForging "VoltairePrototype" <$> shelleyBased

    blockForgingShelleyBased :: m [OptNP 'False (BlockForging m) (VoltairePrototypeEras c)]
    blockForgingShelleyBased = do
        shelleyBased <-
          traverse
            (shelleySharedBlockForging (Proxy @(ShelleyBasedVoltairePrototypeEras c)) tpraosParams)
            credssShelleyBased
        return $ reassoc <$> shelleyBased
      where
        reassoc ::
             NP (BlockForging m :.: ShelleyBlock) (ShelleyBasedVoltairePrototypeEras c)
          -> OptNP 'False (BlockForging m) (VoltairePrototypeEras c)
        reassoc = injectShelleyOptNP unComp . OptNP.fromNonEmptyNP

protocolClientInfoVoltairePrototype
  :: forall c.  ProtocolClientInfo (VoltairePrototypeBlock c)
protocolClientInfoVoltairePrototype = ProtocolClientInfo {
      pClientInfoCodecConfig =
        VoltairePrototypeCodecConfig
          (pClientInfoCodecConfig protocolClientInfoShelley)
          (pClientInfoCodecConfig protocolClientInfoShelley)
          (pClientInfoCodecConfig protocolClientInfoShelley)
    }

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

mkPartialLedgerConfigShelley ::
     ShelleyGenesis era
  -> MaxMajorProtVer
  -> TriggerHardFork
  -> PartialLedgerConfig (ShelleyBlock era)
mkPartialLedgerConfigShelley genesisShelley maxMajorProtVer shelleyTriggerHardFork =
    ShelleyPartialLedgerConfig {
          shelleyLedgerConfig =
            Shelley.mkShelleyLedgerConfig
              genesisShelley
              -- 'completeLedgerConfig' will replace the 'History.dummyEpochInfo'
              -- in the partial ledger config with the correct one.
              History.dummyEpochInfo
              maxMajorProtVer
        , shelleyTriggerHardFork = shelleyTriggerHardFork
        }
