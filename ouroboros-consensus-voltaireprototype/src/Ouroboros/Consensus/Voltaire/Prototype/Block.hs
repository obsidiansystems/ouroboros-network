{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE ViewPatterns             #-}
module Ouroboros.Consensus.Voltaire.Prototype.Block (
    -- * Eras
    -- XXX
    VoltairePrototypeEras
  , module Ouroboros.Consensus.Voltaire.Prototype.Eras
  , ShelleyBasedVoltairePrototypeEras
    -- * Block
  , VoltairePrototypeBlock
    -- Note: by exporting the pattern synonyms as part of the matching data
    -- type (instead of as separate patterns), we get better exhaustiveness
    -- checks from GHC. But GHC expects a data type, not a type family, that's
    -- why we sometimes mention the data type of the instance in these exports
    -- instead of the abstract type family.
  , HardForkBlock (BlockShelley, BlockVoltairePrototype)
    -- * Headers
  , VoltairePrototypeHeader
  , Header (HeaderShelley, HeaderVoltairePrototype)
    -- * Generalised transactions
  , VoltairePrototypeApplyTxErr
  , VoltairePrototypeGenTx
  , VoltairePrototypeGenTxId
  , GenTx (GenTxShelley, GenTxVoltairePrototype)
  , HardForkApplyTxErr (ApplyTxErrShelley, ApplyTxErrVoltairePrototype, ApplyTxErrWrongEra)
  , TxId (GenTxIdShelley, GenTxIdVoltairePrototype)
    -- * LedgerError
  , VoltairePrototypeLedgerError
  , HardForkLedgerError (LedgerErrorShelley, LedgerErrorVoltairePrototype, LedgerErrorWrongEra)
    -- * OtherEnvelopeError
  , VoltairePrototypeOtherHeaderEnvelopeError
  , HardForkEnvelopeErr (OtherHeaderEnvelopeErrorShelley, OtherHeaderEnvelopeErrorVoltairePrototype, OtherHeaderEnvelopeErrorWrongEra)
    -- * TipInfo
  , VoltairePrototypeTipInfo
  , OneEraTipInfo (TipInfoShelley, TipInfoVoltairePrototype)
    -- * Query
  , Either (QueryResultSuccess, QueryResultEraMismatch)
  , VoltairePrototypeQuery
  , VoltairePrototypeQueryResult
  , Query (QueryIfCurrentShelley, QueryIfCurrentVoltairePrototype, QueryAnytimeShelley, QueryAnytimeVoltairePrototype, QueryHardFork)
    -- * CodecConfig
  , CodecConfig (VoltairePrototypeCodecConfig)
  , VoltairePrototypeCodecConfig
    -- * BlockConfig
  , BlockConfig (VoltairePrototypeBlockConfig)
  , VoltairePrototypeBlockConfig
    -- * StorageConfig
  , VoltairePrototypeStorageConfig
  , StorageConfig (VoltairePrototypeStorageConfig)
    -- * ConsensusConfig
  , ConsensusConfig (VoltairePrototypeConsensusConfig)
  , VoltairePrototypeConsensusConfig
    -- * LedgerConfig
  , VoltairePrototypeLedgerConfig
  , HardForkLedgerConfig (VoltairePrototypeLedgerConfig)
    -- * LedgerState
  , VoltairePrototypeLedgerState
  , LedgerState (LedgerStateShelley, LedgerStateVoltairePrototype)
    -- * ChainDepState
  , VoltairePrototypeChainDepState
  , HardForkState (ChainDepStateShelley, ChainDepStateVoltairePrototype)
    -- * EraMismatch
  , EraMismatch (..)
  ) where

import           Data.SOP.Strict

import           Ouroboros.Consensus.Block (BlockProtocol)
import           Ouroboros.Consensus.HeaderValidation (OtherHeaderEnvelopeError,
                     TipInfo)
import           Ouroboros.Consensus.Ledger.Abstract (LedgerError)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr,
                     GenTxId)
import           Ouroboros.Consensus.Protocol.Abstract (ChainDepState)
import           Ouroboros.Consensus.TypeFamilyWrappers

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State

import           Ouroboros.Consensus.Voltaire.Prototype.Eras
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)

{-------------------------------------------------------------------------------
  The eras of the VoltairePrototype block chain
-------------------------------------------------------------------------------}

-- | The eras in the VoltairePrototype blockchain.
--
-- We parameterise over the crypto: @c@.
type VoltairePrototypeEras proto c =
  '[ ShelleyBlock (ShelleyEra c)
   , ShelleyBlock (VoltairePrototypeEra proto c)
   ]

type ShelleyBasedVoltairePrototypeEras proto c =
  '[ ShelleyEra c
   , VoltairePrototypeEra proto c
   ]

{-------------------------------------------------------------------------------
  The block type of the VoltairePrototype block chain
-------------------------------------------------------------------------------}

-- | /The/ VoltairePrototype block.
--
-- Thanks to the pattern synonyms, you can treat this as a sum type with
-- constructors 'BlockShelley' and 'BlockVoltairePrototype'.
--
-- > f :: VoltairePrototypeBlock c -> _
-- > f (BlockShelley s) = _
-- > f (BlockVoltairePrototype a) = _
--
type VoltairePrototypeBlock proto c = HardForkBlock (VoltairePrototypeEras proto c)

pattern BlockShelley :: ShelleyBlock (ShelleyEra c) -> VoltairePrototypeBlock proto c
pattern BlockShelley b = HardForkBlock (OneEraBlock (Z (I b)))

pattern BlockVoltairePrototype :: ShelleyBlock (VoltairePrototypeEra proto c) -> VoltairePrototypeBlock proto c
pattern BlockVoltairePrototype b = HardForkBlock (OneEraBlock (S (Z (I b))))

{-# COMPLETE BlockShelley, BlockVoltairePrototype #-}

{-------------------------------------------------------------------------------
  Headers
-------------------------------------------------------------------------------}

-- | The VoltairePrototype header.
type VoltairePrototypeHeader proto c = Header (VoltairePrototypeBlock proto c)

pattern HeaderShelley ::
     Header (ShelleyBlock (ShelleyEra c))
  -> VoltairePrototypeHeader proto c
pattern HeaderShelley h = HardForkHeader (OneEraHeader (Z h))

pattern HeaderVoltairePrototype ::
     Header (ShelleyBlock (VoltairePrototypeEra proto c))
  -> VoltairePrototypeHeader proto c
pattern HeaderVoltairePrototype h = HardForkHeader (OneEraHeader (S (Z h)))

{-# COMPLETE HeaderShelley, HeaderVoltairePrototype #-}

{-------------------------------------------------------------------------------
  Generalised transactions
-------------------------------------------------------------------------------}

-- | The VoltairePrototype transaction.
type VoltairePrototypeGenTx proto c = GenTx (VoltairePrototypeBlock proto c)

pattern GenTxShelley :: GenTx (ShelleyBlock (ShelleyEra c)) -> VoltairePrototypeGenTx proto c
pattern GenTxShelley tx = HardForkGenTx (OneEraGenTx (Z tx))

pattern GenTxVoltairePrototype :: GenTx (ShelleyBlock (VoltairePrototypeEra proto c)) -> VoltairePrototypeGenTx proto c
pattern GenTxVoltairePrototype tx = HardForkGenTx (OneEraGenTx (S (Z tx)))

{-# COMPLETE GenTxShelley, GenTxVoltairePrototype #-}

-- | The ID of an VoltairePrototype transaction.
type VoltairePrototypeGenTxId proto c = GenTxId (VoltairePrototypeBlock proto c)

pattern GenTxIdShelley ::
     GenTxId (ShelleyBlock (ShelleyEra c))
  -> VoltairePrototypeGenTxId proto c
pattern GenTxIdShelley txid =
    HardForkGenTxId (OneEraGenTxId (Z (WrapGenTxId txid)))

pattern GenTxIdVoltairePrototype ::
     GenTxId (ShelleyBlock (VoltairePrototypeEra proto c))
  -> VoltairePrototypeGenTxId proto c
pattern GenTxIdVoltairePrototype txid =
    HardForkGenTxId (OneEraGenTxId (S (Z (WrapGenTxId txid))))

{-# COMPLETE GenTxIdShelley, GenTxIdVoltairePrototype #-}

-- | An error resulting from applying a 'VoltairePrototypeGenTx' to the ledger.
--
-- Thanks to the pattern synonyms, you can treat this as a sum type with
-- constructors 'ApplyTxErrShelley', 'ApplyTxErrVoltairePrototype', and
-- 'ApplyTxErrWrongEra'.
--
-- > toText :: VoltairePrototypeApplyTxErr c -> Text
-- > toText (ApplyTxErrShelley s) = shelleyApplyTxErrToText s
-- > tlText (ApplyTxErrVoltairePrototype a) = exampleApplyTxErrToText a
-- > toText (ApplyTxErrWrongEra eraMismatch) =
-- >   "Transaction from the " <> otherEraName eraMismatch <>
-- >   " era applied to a ledger from the " <>
-- >   ledgerEraName eraMismatch <> " era"
--
type VoltairePrototypeApplyTxErr proto c = HardForkApplyTxErr (VoltairePrototypeEras proto c)

pattern ApplyTxErrShelley ::
     ApplyTxErr (ShelleyBlock (ShelleyEra c))
  -> VoltairePrototypeApplyTxErr proto c
pattern ApplyTxErrShelley err =
    HardForkApplyTxErrFromEra (OneEraApplyTxErr (Z (WrapApplyTxErr err)))

pattern ApplyTxErrVoltairePrototype ::
     ApplyTxErr (ShelleyBlock (VoltairePrototypeEra proto c))
  -> VoltairePrototypeApplyTxErr proto c
pattern ApplyTxErrVoltairePrototype err =
    HardForkApplyTxErrFromEra (OneEraApplyTxErr (S (Z (WrapApplyTxErr err))))

pattern ApplyTxErrWrongEra :: EraMismatch -> VoltairePrototypeApplyTxErr proto c
pattern ApplyTxErrWrongEra eraMismatch <-
    HardForkApplyTxErrWrongEra (mkEraMismatch -> eraMismatch)

{-# COMPLETE ApplyTxErrShelley
           , ApplyTxErrVoltairePrototype
           , ApplyTxErrWrongEra #-}

{-------------------------------------------------------------------------------
  LedgerError
-------------------------------------------------------------------------------}

-- | An error resulting from applying a 'VoltairePrototypeBlock' to the ledger.
--
-- Thanks to the pattern synonyms, you can treat this as a sum type with
-- constructors 'LedgerErrorShelley', 'LedgerErrorVoltairePrototype', and
-- 'LedgerErrorWrongEra'.
--
-- > toText :: VoltairePrototypeLedgerError c -> Text
-- > toText (LedgerErrorShelley s) = shelleyLedgerErrorToText s
-- > toText (LedgerErrorVoltairePrototype a) = allegraLedgerErrorToText a
-- > toText (LedgerErrorWrongEra eraMismatch) =
-- >   "Block from the " <> otherEraName eraMismatch <>
-- >   " era applied to a ledger from the " <>
-- >   ledgerEraName eraMismatch <> " era"
--
type VoltairePrototypeLedgerError proto c = HardForkLedgerError (VoltairePrototypeEras proto c)

pattern LedgerErrorShelley ::
     LedgerError (ShelleyBlock (ShelleyEra c))
  -> VoltairePrototypeLedgerError proto c
pattern LedgerErrorShelley err =
    HardForkLedgerErrorFromEra
      (OneEraLedgerError (Z (WrapLedgerErr err)))

pattern LedgerErrorVoltairePrototype ::
     LedgerError (ShelleyBlock (VoltairePrototypeEra proto c))
  -> VoltairePrototypeLedgerError proto c
pattern LedgerErrorVoltairePrototype err =
    HardForkLedgerErrorFromEra
      (OneEraLedgerError (S (Z (WrapLedgerErr err))))

pattern LedgerErrorWrongEra :: EraMismatch -> VoltairePrototypeLedgerError proto c
pattern LedgerErrorWrongEra eraMismatch <-
    HardForkLedgerErrorWrongEra (mkEraMismatch -> eraMismatch)

{-# COMPLETE LedgerErrorShelley
           , LedgerErrorVoltairePrototype
           , LedgerErrorWrongEra #-}

{-------------------------------------------------------------------------------
  OtherEnvelopeError
-------------------------------------------------------------------------------}

-- | An error resulting from validating a 'VoltairePrototypeHeader'.
type VoltairePrototypeOtherHeaderEnvelopeError proto c = HardForkEnvelopeErr (VoltairePrototypeEras proto c)

pattern OtherHeaderEnvelopeErrorShelley
  :: OtherHeaderEnvelopeError (ShelleyBlock (ShelleyEra c))
  -> VoltairePrototypeOtherHeaderEnvelopeError proto c
pattern OtherHeaderEnvelopeErrorShelley err =
    HardForkEnvelopeErrFromEra (OneEraEnvelopeErr (Z (WrapEnvelopeErr err)))

pattern OtherHeaderEnvelopeErrorVoltairePrototype
  :: OtherHeaderEnvelopeError (ShelleyBlock (VoltairePrototypeEra proto c))
  -> VoltairePrototypeOtherHeaderEnvelopeError proto c
pattern OtherHeaderEnvelopeErrorVoltairePrototype err =
    HardForkEnvelopeErrFromEra (OneEraEnvelopeErr (S (Z (WrapEnvelopeErr err))))

pattern OtherHeaderEnvelopeErrorWrongEra
  :: EraMismatch
  -> VoltairePrototypeOtherHeaderEnvelopeError proto c
pattern OtherHeaderEnvelopeErrorWrongEra eraMismatch <-
    HardForkEnvelopeErrWrongEra (mkEraMismatch -> eraMismatch)

{-# COMPLETE OtherHeaderEnvelopeErrorShelley
           , OtherHeaderEnvelopeErrorVoltairePrototype
           , OtherHeaderEnvelopeErrorWrongEra #-}

{-------------------------------------------------------------------------------
  TipInfo
-------------------------------------------------------------------------------}

-- | The 'TipInfo' of the VoltairePrototype chain.
type VoltairePrototypeTipInfo proto c = OneEraTipInfo (VoltairePrototypeEras proto c)

pattern TipInfoShelley ::
     TipInfo (ShelleyBlock (ShelleyEra c))
  -> VoltairePrototypeTipInfo proto c
pattern TipInfoShelley ti = OneEraTipInfo (Z (WrapTipInfo ti))

pattern TipInfoVoltairePrototype ::
     TipInfo (ShelleyBlock (VoltairePrototypeEra proto c))
  -> VoltairePrototypeTipInfo proto c
pattern TipInfoVoltairePrototype ti = OneEraTipInfo (S (Z (WrapTipInfo ti)))

{-# COMPLETE TipInfoShelley, TipInfoVoltairePrototype #-}

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

-- | The 'Query' of VoltairePrototype chain.
type VoltairePrototypeQuery proto c = Query (VoltairePrototypeBlock proto c)

-- | Shelley-specific query that can only be answered when the ledger is in the
-- Shelley era.
pattern QueryIfCurrentShelley
  :: ()
  => VoltairePrototypeQueryResult proto c result ~ a
  => Query (ShelleyBlock (ShelleyEra c)) result
  -> VoltairePrototypeQuery proto c a
pattern QueryIfCurrentShelley q = QueryIfCurrent (QZ q)

-- | VoltairePrototype-specific query that can only be answered when the ledger is in the
-- VoltairePrototype era.
pattern QueryIfCurrentVoltairePrototype
  :: ()
  => VoltairePrototypeQueryResult proto c result ~ a
  => Query (ShelleyBlock (VoltairePrototypeEra proto c)) result
  -> VoltairePrototypeQuery proto c a
pattern QueryIfCurrentVoltairePrototype q = QueryIfCurrent (QS (QZ q))

-- | Query about the Shelley era that can be answered anytime, i.e.,
-- independent from where the tip of the ledger is.
--
-- For example, to ask for the start of the Shelley era (whether the tip of the
-- ledger is in the Shelley, VoltairePrototype, ... era), use:
--
-- > QueryAnytimeShelley EraStart
--
pattern QueryAnytimeShelley
  :: QueryAnytime result
  -> VoltairePrototypeQuery proto c result
pattern QueryAnytimeShelley q = QueryAnytime q (EraIndex (Z (K ())))

-- | Query about the Shelley era that can be answered anytime, i.e.,
-- independent from where the tip of the ledger is.
--
-- For example, to ask for the start of the Shelley era (whether the tip of the
-- ledger is in the Shelley, VoltairePrototype, ... era), use:
--
-- > QueryAnytimeShelley EraStart
--
pattern QueryAnytimeVoltairePrototype
  :: QueryAnytime result
  -> VoltairePrototypeQuery proto c result
pattern QueryAnytimeVoltairePrototype q = QueryAnytime q (EraIndex (S (Z (K ()))))

{-# COMPLETE QueryIfCurrentShelley
           , QueryIfCurrentVoltairePrototype
           , QueryAnytimeShelley
           , QueryAnytimeVoltairePrototype
           , QueryHardFork #-}

-- | The result of a 'VoltairePrototypeQuery'
--
-- Thanks to the pattern synonyms, you can treat this as a sum type with
-- constructors 'QueryResultSuccess' and 'QueryResultEraMismatch'.
type VoltairePrototypeQueryResult proto c = HardForkQueryResult (VoltairePrototypeEras proto c)

pattern QueryResultSuccess :: result -> VoltairePrototypeQueryResult proto c result
pattern QueryResultSuccess result = Right result

-- | A query from a different era than the ledger's era was sent.
pattern QueryResultEraMismatch :: EraMismatch -> VoltairePrototypeQueryResult proto c result
pattern QueryResultEraMismatch eraMismatch <- Left (mkEraMismatch -> eraMismatch)

{-# COMPLETE QueryResultSuccess, QueryResultEraMismatch #-}

{-------------------------------------------------------------------------------
  CodecConfig
-------------------------------------------------------------------------------}

-- | The 'CodecConfig' for 'VoltairePrototypeBlock'.
--
-- Thanks to the pattern synonyms, you can treat this as the product of
-- the Shelley, VoltairePrototype ... 'CodecConfig's.
type VoltairePrototypeCodecConfig proto c = CodecConfig (VoltairePrototypeBlock proto c)

pattern VoltairePrototypeCodecConfig
  :: CodecConfig (ShelleyBlock (ShelleyEra c))
  -> CodecConfig (ShelleyBlock (VoltairePrototypeEra proto c))
  -> VoltairePrototypeCodecConfig proto c
pattern VoltairePrototypeCodecConfig cfgShelley cfgVoltairePrototype =
    HardForkCodecConfig {
        hardForkCodecConfigPerEra = PerEraCodecConfig
          (  cfgShelley
          :* cfgVoltairePrototype
          :* Nil
          )
      }

{-# COMPLETE VoltairePrototypeCodecConfig #-}

{-------------------------------------------------------------------------------
  BlockConfig
-------------------------------------------------------------------------------}

-- | The 'BlockConfig' for 'VoltairePrototypeBlock'.
--
-- Thanks to the pattern synonyms, you can treat this as the product of
-- the Shelley, VoltairePrototype, ... 'BlockConfig's.
type VoltairePrototypeBlockConfig proto c = BlockConfig (VoltairePrototypeBlock proto c)

pattern VoltairePrototypeBlockConfig
  :: BlockConfig (ShelleyBlock (ShelleyEra c))
  -> BlockConfig (ShelleyBlock (VoltairePrototypeEra proto c))
  -> VoltairePrototypeBlockConfig proto c
pattern VoltairePrototypeBlockConfig cfgShelley cfgVoltairePrototype =
    HardForkBlockConfig {
        hardForkBlockConfigPerEra = PerEraBlockConfig
          (  cfgShelley
          :* cfgVoltairePrototype
          :* Nil
          )
      }

{-# COMPLETE VoltairePrototypeBlockConfig #-}

{-------------------------------------------------------------------------------
  StorageConfig
-------------------------------------------------------------------------------}

-- | The 'StorageConfig' for 'VoltairePrototypeBlock'.
--
-- Thanks to the pattern synonyms, you can treat this as the product of
-- the Shelley, VoltairePrototype, ... 'StorageConfig's.
type VoltairePrototypeStorageConfig proto c = StorageConfig (VoltairePrototypeBlock proto c)

pattern VoltairePrototypeStorageConfig
  :: StorageConfig (ShelleyBlock (ShelleyEra c))
  -> StorageConfig (ShelleyBlock (VoltairePrototypeEra proto c))
  -> VoltairePrototypeStorageConfig proto c
pattern VoltairePrototypeStorageConfig cfgShelley cfgVoltairePrototype =
    HardForkStorageConfig {
        hardForkStorageConfigPerEra = PerEraStorageConfig
          (  cfgShelley
          :* cfgVoltairePrototype
          :* Nil
          )
      }

{-# COMPLETE VoltairePrototypeStorageConfig #-}

{-------------------------------------------------------------------------------
  ConsensusConfig
-------------------------------------------------------------------------------}

-- | The 'ConsensusConfig' for 'VoltairePrototypeBlock'.
--
-- Thanks to the pattern synonyms, you can treat this as the product of the
-- Shelley, VoltairePrototype, ... 'PartialConsensusConfig's.
--
-- NOTE: not 'ConsensusConfig', but 'PartialConsensusConfig'.
type VoltairePrototypeConsensusConfig proto c =
  ConsensusConfig (HardForkProtocol (VoltairePrototypeEras proto c))

pattern VoltairePrototypeConsensusConfig
  :: PartialConsensusConfig (BlockProtocol (ShelleyBlock (ShelleyEra c)))
  -> PartialConsensusConfig (BlockProtocol (ShelleyBlock (VoltairePrototypeEra proto c)))
  -> VoltairePrototypeConsensusConfig proto c
pattern VoltairePrototypeConsensusConfig cfgShelley cfgVoltairePrototype <-
    HardForkConsensusConfig {
        hardForkConsensusConfigPerEra = PerEraConsensusConfig
          (  WrapPartialConsensusConfig cfgShelley
          :* WrapPartialConsensusConfig cfgVoltairePrototype
          :* Nil
          )
      }

{-# COMPLETE VoltairePrototypeConsensusConfig #-}

{-------------------------------------------------------------------------------
  LedgerConfig
-------------------------------------------------------------------------------}

-- | The 'LedgerConfig' for 'VoltairePrototypeBlock'.
--
-- Thanks to the pattern synonyms, you can treat this as the product of the
-- Shelley, VoltairePrototype, ... 'PartialLedgerConfig's.
--
-- NOTE: not 'LedgerConfig', but 'PartialLedgerConfig'.
type VoltairePrototypeLedgerConfig proto c = HardForkLedgerConfig (VoltairePrototypeEras proto c)

pattern VoltairePrototypeLedgerConfig
  :: PartialLedgerConfig (ShelleyBlock (ShelleyEra c))
  -> PartialLedgerConfig (ShelleyBlock (VoltairePrototypeEra proto c))
  -> VoltairePrototypeLedgerConfig proto c
pattern VoltairePrototypeLedgerConfig cfgShelley cfgVoltairePrototype <-
    HardForkLedgerConfig {
        hardForkLedgerConfigPerEra = PerEraLedgerConfig
          ( WrapPartialLedgerConfig cfgShelley
          :* WrapPartialLedgerConfig cfgVoltairePrototype
          :* Nil
          )
      }

{-# COMPLETE VoltairePrototypeLedgerConfig #-}

{-------------------------------------------------------------------------------
  LedgerState
-------------------------------------------------------------------------------}

-- | The 'LedgerState' for 'VoltairePrototypeBlock'.
--
-- NOTE: the 'VoltairePrototypeLedgerState' contains more than just the current era's
-- 'LedgerState'. We don't give access to those internal details through the
-- pattern synonyms. This is also the reason the pattern synonyms are not
-- bidirectional.
type VoltairePrototypeLedgerState proto c = LedgerState (VoltairePrototypeBlock proto c)

pattern LedgerStateShelley
  :: LedgerState (ShelleyBlock (ShelleyEra c))
  -> VoltairePrototypeLedgerState proto c
pattern LedgerStateShelley st <-
    HardForkLedgerState
      (State.HardForkState
        (TZ (State.Current { currentState = st })))

pattern LedgerStateVoltairePrototype
  :: LedgerState (ShelleyBlock (VoltairePrototypeEra proto c))
  -> VoltairePrototypeLedgerState proto c
pattern LedgerStateVoltairePrototype st <-
    HardForkLedgerState
      (State.HardForkState
        (TS _ (TZ (State.Current { currentState = st }))))

{-# COMPLETE LedgerStateShelley
           , LedgerStateVoltairePrototype #-}

{-------------------------------------------------------------------------------
  ChainDepState
-------------------------------------------------------------------------------}

-- | The 'ChainDepState' for 'VoltairePrototypeBlock'.
--
-- NOTE: the 'VoltairePrototypeChainDepState' contains more than just the current era's
-- 'ChainDepState'. We don't give access to those internal details through the
-- pattern synonyms. This is also the reason the pattern synonyms are not
-- bidirectional.
type VoltairePrototypeChainDepState proto c = HardForkChainDepState (VoltairePrototypeEras proto c)

pattern ChainDepStateShelley
  :: ChainDepState (BlockProtocol (ShelleyBlock (ShelleyEra c)))
  -> VoltairePrototypeChainDepState proto c
pattern ChainDepStateShelley st <-
    State.HardForkState
      (TZ (State.Current { currentState = WrapChainDepState st }))

pattern ChainDepStateVoltairePrototype
  :: ChainDepState (BlockProtocol (ShelleyBlock (VoltairePrototypeEra proto c)))
  -> VoltairePrototypeChainDepState proto c
pattern ChainDepStateVoltairePrototype st <-
    State.HardForkState
      (TS _ (TZ (State.Current { currentState = WrapChainDepState st })))

{-# COMPLETE ChainDepStateShelley
           , ChainDepStateVoltairePrototype #-}
