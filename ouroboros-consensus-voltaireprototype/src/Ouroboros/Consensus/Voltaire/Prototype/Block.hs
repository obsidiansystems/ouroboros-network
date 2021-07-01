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
  , HardForkBlock (BlockShelley, BlockVoltairePrototypeOne, BlockVoltairePrototypeTwo)
    -- * Headers
  , VoltairePrototypeHeader
  , Header (HeaderShelley, HeaderVoltairePrototypeOne, HeaderVoltairePrototypeTwo)
    -- * Generalised transactions
  , VoltairePrototypeApplyTxErr
  , VoltairePrototypeGenTx
  , VoltairePrototypeGenTxId
  , GenTx (GenTxShelley, GenTxVoltairePrototypeOne, GenTxVoltairePrototypeTwo)
  , HardForkApplyTxErr (ApplyTxErrShelley, ApplyTxErrVoltairePrototypeOne, ApplyTxErrVoltairePrototypeTwo, ApplyTxErrWrongEra)
  , TxId (GenTxIdShelley, GenTxIdVoltairePrototypeOne, GenTxIdVoltairePrototypeTwo)
    -- * LedgerError
  , VoltairePrototypeLedgerError
  , HardForkLedgerError (LedgerErrorShelley, LedgerErrorVoltairePrototypeOne, LedgerErrorVoltairePrototypeTwo, LedgerErrorWrongEra)
    -- * OtherEnvelopeError
  , VoltairePrototypeOtherHeaderEnvelopeError
  , HardForkEnvelopeErr (OtherHeaderEnvelopeErrorShelley, OtherHeaderEnvelopeErrorVoltairePrototypeOne, OtherHeaderEnvelopeErrorVoltairePrototypeTwo, OtherHeaderEnvelopeErrorWrongEra)
    -- * TipInfo
  , VoltairePrototypeTipInfo
  , OneEraTipInfo (TipInfoShelley, TipInfoVoltairePrototypeOne, TipInfoVoltairePrototypeTwo)
    -- * Query
  , Either (QueryResultSuccess, QueryResultEraMismatch)
  , VoltairePrototypeQuery
  , VoltairePrototypeQueryResult
  , Query (QueryIfCurrentShelley, QueryIfCurrentVoltairePrototypeOne, QueryIfCurrentVoltairePrototypeTwo, QueryAnytimeShelley, QueryAnytimeVoltairePrototypeOne, QueryAnytimeVoltairePrototypeTwo, QueryHardFork)
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
  , LedgerState (LedgerStateShelley, LedgerStateVoltairePrototypeOne, LedgerStateVoltairePrototypeTwo)
    -- * ChainDepState
  , VoltairePrototypeChainDepState
  , HardForkState (ChainDepStateShelley, ChainDepStateVoltairePrototypeOne, ChainDepStateVoltairePrototypeTwo)
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
type VoltairePrototypeEras c =
  '[ ShelleyBlock (ShelleyEra c)
   , ShelleyBlock (VoltairePrototypeEra 'VoltairePrototype_One c)
   , ShelleyBlock (VoltairePrototypeEra 'VoltairePrototype_Two c)
   ]

type ShelleyBasedVoltairePrototypeEras c =
  '[ ShelleyEra c
   , VoltairePrototypeEra 'VoltairePrototype_One c
   , VoltairePrototypeEra 'VoltairePrototype_Two c
   ]

{-------------------------------------------------------------------------------
  The block type of the VoltairePrototype block chain
-------------------------------------------------------------------------------}

-- | /The/ VoltairePrototype block.
--
-- Thanks to the pattern synonyms, you can treat this as a sum type with
-- constructors 'BlockShelley', 'BlockVoltairePrototypeOne', and
-- 'BlockVoltairePrototypeTwo'.
--
-- > f :: VoltairePrototypeBlock c -> _
-- > f (BlockShelley s) = _
-- > f (BlockVoltairePrototypeOne a) = _
-- > f (BlockVoltairePrototypeTwo a) = _
--
type VoltairePrototypeBlock c = HardForkBlock (VoltairePrototypeEras c)

pattern BlockShelley :: ShelleyBlock (ShelleyEra c) -> VoltairePrototypeBlock c
pattern BlockShelley b = HardForkBlock (OneEraBlock (Z (I b)))

pattern BlockVoltairePrototypeOne :: ShelleyBlock (VoltairePrototypeEra 'VoltairePrototype_One c)
                                  -> VoltairePrototypeBlock c
pattern BlockVoltairePrototypeOne b = HardForkBlock (OneEraBlock (S (Z (I b))))

pattern BlockVoltairePrototypeTwo :: ShelleyBlock (VoltairePrototypeEra 'VoltairePrototype_Two c)
                                  -> VoltairePrototypeBlock c
pattern BlockVoltairePrototypeTwo b = HardForkBlock (OneEraBlock (S (S (Z (I b)))))

{-# COMPLETE BlockShelley, BlockVoltairePrototypeOne, BlockVoltairePrototypeTwo #-}

{-------------------------------------------------------------------------------
  Headers
-------------------------------------------------------------------------------}

-- | The VoltairePrototype header.
type VoltairePrototypeHeader c = Header (VoltairePrototypeBlock c)

pattern HeaderShelley ::
     Header (ShelleyBlock (ShelleyEra c))
  -> VoltairePrototypeHeader c
pattern HeaderShelley h = HardForkHeader (OneEraHeader (Z h))

pattern HeaderVoltairePrototypeOne ::
     Header (ShelleyBlock (VoltairePrototypeEra 'VoltairePrototype_One c))
  -> VoltairePrototypeHeader c
pattern HeaderVoltairePrototypeOne h = HardForkHeader (OneEraHeader (S (Z h)))

pattern HeaderVoltairePrototypeTwo ::
     Header (ShelleyBlock (VoltairePrototypeEra 'VoltairePrototype_Two c))
  -> VoltairePrototypeHeader c
pattern HeaderVoltairePrototypeTwo h = HardForkHeader (OneEraHeader (S (S (Z h))))

{-# COMPLETE HeaderShelley, HeaderVoltairePrototypeOne, HeaderVoltairePrototypeTwo #-}

{-------------------------------------------------------------------------------
  Generalised transactions
-------------------------------------------------------------------------------}

-- | The VoltairePrototype transaction.
type VoltairePrototypeGenTx c = GenTx (VoltairePrototypeBlock c)

pattern GenTxShelley :: GenTx (ShelleyBlock (ShelleyEra c)) -> VoltairePrototypeGenTx c
pattern GenTxShelley tx = HardForkGenTx (OneEraGenTx (Z tx))

pattern GenTxVoltairePrototypeOne :: GenTx (ShelleyBlock (VoltairePrototypeEra 'VoltairePrototype_One c))
                                  -> VoltairePrototypeGenTx c
pattern GenTxVoltairePrototypeOne tx = HardForkGenTx (OneEraGenTx (S (Z tx)))

pattern GenTxVoltairePrototypeTwo :: GenTx (ShelleyBlock (VoltairePrototypeEra 'VoltairePrototype_Two c))
                                  -> VoltairePrototypeGenTx c
pattern GenTxVoltairePrototypeTwo tx = HardForkGenTx (OneEraGenTx (S (S (Z tx))))

{-# COMPLETE GenTxShelley, GenTxVoltairePrototypeOne, GenTxVoltairePrototypeTwo #-}

-- | The ID of an VoltairePrototype transaction.
type VoltairePrototypeGenTxId c = GenTxId (VoltairePrototypeBlock c)

pattern GenTxIdShelley ::
     GenTxId (ShelleyBlock (ShelleyEra c))
  -> VoltairePrototypeGenTxId c
pattern GenTxIdShelley txid =
    HardForkGenTxId (OneEraGenTxId (Z (WrapGenTxId txid)))

pattern GenTxIdVoltairePrototypeOne ::
     GenTxId (ShelleyBlock (VoltairePrototypeEra 'VoltairePrototype_One c))
  -> VoltairePrototypeGenTxId c
pattern GenTxIdVoltairePrototypeOne txid =
    HardForkGenTxId (OneEraGenTxId (S (Z (WrapGenTxId txid))))

pattern GenTxIdVoltairePrototypeTwo ::
     GenTxId (ShelleyBlock (VoltairePrototypeEra 'VoltairePrototype_Two c))
  -> VoltairePrototypeGenTxId c
pattern GenTxIdVoltairePrototypeTwo txid =
    HardForkGenTxId (OneEraGenTxId (S (S (Z (WrapGenTxId txid)))))

{-# COMPLETE GenTxIdShelley, GenTxIdVoltairePrototypeOne, GenTxIdVoltairePrototypeTwo #-}

-- | An error resulting from applying a 'VoltairePrototypeGenTx' to the ledger.
--
-- Thanks to the pattern synonyms, you can treat this as a sum type with
-- constructors 'ApplyTxErrShelley', 'ApplyTxErrVoltairePrototypeOne', and
-- 'ApplyTxErrWrongEra'.
--
-- > toText :: VoltairePrototypeApplyTxErr c -> Text
-- > toText (ApplyTxErrShelley s) = shelleyApplyTxErrToText s
-- > tlText (ApplyTxErrVoltairePrototypeOne a) = exampleApplyTxErrToText a
-- > toText (ApplyTxErrWrongEra eraMismatch) =
-- >   "Transaction from the " <> otherEraName eraMismatch <>
-- >   " era applied to a ledger from the " <>
-- >   ledgerEraName eraMismatch <> " era"
--
type VoltairePrototypeApplyTxErr c = HardForkApplyTxErr (VoltairePrototypeEras c)

pattern ApplyTxErrShelley ::
     ApplyTxErr (ShelleyBlock (ShelleyEra c))
  -> VoltairePrototypeApplyTxErr c
pattern ApplyTxErrShelley err =
    HardForkApplyTxErrFromEra (OneEraApplyTxErr (Z (WrapApplyTxErr err)))

pattern ApplyTxErrVoltairePrototypeOne ::
     ApplyTxErr (ShelleyBlock (VoltairePrototypeEra 'VoltairePrototype_One c))
  -> VoltairePrototypeApplyTxErr c
pattern ApplyTxErrVoltairePrototypeOne err =
    HardForkApplyTxErrFromEra (OneEraApplyTxErr (S (Z (WrapApplyTxErr err))))

pattern ApplyTxErrVoltairePrototypeTwo ::
     ApplyTxErr (ShelleyBlock (VoltairePrototypeEra 'VoltairePrototype_Two c))
  -> VoltairePrototypeApplyTxErr c
pattern ApplyTxErrVoltairePrototypeTwo err =
    HardForkApplyTxErrFromEra (OneEraApplyTxErr (S (S (Z (WrapApplyTxErr err)))))

pattern ApplyTxErrWrongEra :: EraMismatch -> VoltairePrototypeApplyTxErr c
pattern ApplyTxErrWrongEra eraMismatch <-
    HardForkApplyTxErrWrongEra (mkEraMismatch -> eraMismatch)

{-# COMPLETE ApplyTxErrShelley
           , ApplyTxErrVoltairePrototypeOne
           , ApplyTxErrVoltairePrototypeTwo
           , ApplyTxErrWrongEra #-}

{-------------------------------------------------------------------------------
  LedgerError
-------------------------------------------------------------------------------}

-- | An error resulting from applying a 'VoltairePrototypeBlock' to the ledger.
--
-- Thanks to the pattern synonyms, you can treat this as a sum type with
-- constructors 'LedgerErrorShelley', 'LedgerErrorVoltairePrototypeOne', and
-- 'LedgerErrorWrongEra'.
--
-- > toText :: VoltairePrototypeLedgerError c -> Text
-- > toText (LedgerErrorShelley s) = shelleyLedgerErrorToText s
-- > toText (LedgerErrorVoltairePrototypeOne a) = allegraLedgerErrorToText a
-- > toText (LedgerErrorWrongEra eraMismatch) =
-- >   "Block from the " <> otherEraName eraMismatch <>
-- >   " era applied to a ledger from the " <>
-- >   ledgerEraName eraMismatch <> " era"
--
type VoltairePrototypeLedgerError c = HardForkLedgerError (VoltairePrototypeEras c)

pattern LedgerErrorShelley ::
     LedgerError (ShelleyBlock (ShelleyEra c))
  -> VoltairePrototypeLedgerError c
pattern LedgerErrorShelley err =
    HardForkLedgerErrorFromEra
      (OneEraLedgerError (Z (WrapLedgerErr err)))

pattern LedgerErrorVoltairePrototypeOne ::
     LedgerError (ShelleyBlock (VoltairePrototypeEra 'VoltairePrototype_One c))
  -> VoltairePrototypeLedgerError c
pattern LedgerErrorVoltairePrototypeOne err =
    HardForkLedgerErrorFromEra
      (OneEraLedgerError (S (Z (WrapLedgerErr err))))

pattern LedgerErrorVoltairePrototypeTwo ::
     LedgerError (ShelleyBlock (VoltairePrototypeEra 'VoltairePrototype_Two c))
  -> VoltairePrototypeLedgerError c
pattern LedgerErrorVoltairePrototypeTwo err =
    HardForkLedgerErrorFromEra
      (OneEraLedgerError (S (S (Z (WrapLedgerErr err)))))

pattern LedgerErrorWrongEra :: EraMismatch -> VoltairePrototypeLedgerError c
pattern LedgerErrorWrongEra eraMismatch <-
    HardForkLedgerErrorWrongEra (mkEraMismatch -> eraMismatch)

{-# COMPLETE LedgerErrorShelley
           , LedgerErrorVoltairePrototypeOne
           , LedgerErrorVoltairePrototypeTwo
           , LedgerErrorWrongEra #-}

{-------------------------------------------------------------------------------
  OtherEnvelopeError
-------------------------------------------------------------------------------}

-- | An error resulting from validating a 'VoltairePrototypeHeader'.
type VoltairePrototypeOtherHeaderEnvelopeError c = HardForkEnvelopeErr (VoltairePrototypeEras c)

pattern OtherHeaderEnvelopeErrorShelley
  :: OtherHeaderEnvelopeError (ShelleyBlock (ShelleyEra c))
  -> VoltairePrototypeOtherHeaderEnvelopeError c
pattern OtherHeaderEnvelopeErrorShelley err =
    HardForkEnvelopeErrFromEra (OneEraEnvelopeErr (Z (WrapEnvelopeErr err)))

pattern OtherHeaderEnvelopeErrorVoltairePrototypeOne
  :: OtherHeaderEnvelopeError (ShelleyBlock (VoltairePrototypeEra 'VoltairePrototype_One c))
  -> VoltairePrototypeOtherHeaderEnvelopeError c
pattern OtherHeaderEnvelopeErrorVoltairePrototypeOne err =
    HardForkEnvelopeErrFromEra (OneEraEnvelopeErr (S (Z (WrapEnvelopeErr err))))

pattern OtherHeaderEnvelopeErrorVoltairePrototypeTwo
  :: OtherHeaderEnvelopeError (ShelleyBlock (VoltairePrototypeEra 'VoltairePrototype_Two c))
  -> VoltairePrototypeOtherHeaderEnvelopeError c
pattern OtherHeaderEnvelopeErrorVoltairePrototypeTwo err =
    HardForkEnvelopeErrFromEra (OneEraEnvelopeErr (S (S (Z (WrapEnvelopeErr err)))))

pattern OtherHeaderEnvelopeErrorWrongEra
  :: EraMismatch
  -> VoltairePrototypeOtherHeaderEnvelopeError c
pattern OtherHeaderEnvelopeErrorWrongEra eraMismatch <-
    HardForkEnvelopeErrWrongEra (mkEraMismatch -> eraMismatch)

{-# COMPLETE OtherHeaderEnvelopeErrorShelley
           , OtherHeaderEnvelopeErrorVoltairePrototypeOne
           , OtherHeaderEnvelopeErrorVoltairePrototypeTwo
           , OtherHeaderEnvelopeErrorWrongEra #-}

{-------------------------------------------------------------------------------
  TipInfo
-------------------------------------------------------------------------------}

-- | The 'TipInfo' of the VoltairePrototype chain.
type VoltairePrototypeTipInfo c = OneEraTipInfo (VoltairePrototypeEras c)

pattern TipInfoShelley ::
     TipInfo (ShelleyBlock (ShelleyEra c))
  -> VoltairePrototypeTipInfo c
pattern TipInfoShelley ti = OneEraTipInfo (Z (WrapTipInfo ti))

pattern TipInfoVoltairePrototypeOne ::
     TipInfo (ShelleyBlock (VoltairePrototypeEra 'VoltairePrototype_One c))
  -> VoltairePrototypeTipInfo c
pattern TipInfoVoltairePrototypeOne ti = OneEraTipInfo (S (Z (WrapTipInfo ti)))

pattern TipInfoVoltairePrototypeTwo ::
     TipInfo (ShelleyBlock (VoltairePrototypeEra 'VoltairePrototype_Two c))
  -> VoltairePrototypeTipInfo c
pattern TipInfoVoltairePrototypeTwo ti = OneEraTipInfo (S (S (Z (WrapTipInfo ti))))

{-# COMPLETE TipInfoShelley, TipInfoVoltairePrototypeOne, TipInfoVoltairePrototypeTwo #-}

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

-- | The 'Query' of VoltairePrototype chain.
type VoltairePrototypeQuery c = Query (VoltairePrototypeBlock c)

-- | Shelley-specific query that can only be answered when the ledger is in the
-- Shelley era.
pattern QueryIfCurrentShelley
  :: ()
  => VoltairePrototypeQueryResult c result ~ a
  => Query (ShelleyBlock (ShelleyEra c)) result
  -> VoltairePrototypeQuery c a
pattern QueryIfCurrentShelley q = QueryIfCurrent (QZ q)

-- | VoltairePrototype-specific query that can only be answered when the ledger is in the
-- VoltairePrototype_One era.
pattern QueryIfCurrentVoltairePrototypeOne
  :: ()
  => VoltairePrototypeQueryResult c result ~ a
  => Query (ShelleyBlock (VoltairePrototypeEra 'VoltairePrototype_One c)) result
  -> VoltairePrototypeQuery c a
pattern QueryIfCurrentVoltairePrototypeOne q = QueryIfCurrent (QS (QZ q))

-- | VoltairePrototype-specific query that can only be answered when the ledger is in the
-- VoltairePrototype_Two era.
pattern QueryIfCurrentVoltairePrototypeTwo
  :: ()
  => VoltairePrototypeQueryResult c result ~ a
  => Query (ShelleyBlock (VoltairePrototypeEra 'VoltairePrototype_Two c)) result
  -> VoltairePrototypeQuery c a
pattern QueryIfCurrentVoltairePrototypeTwo q = QueryIfCurrent (QS (QS (QZ q)))

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
  -> VoltairePrototypeQuery c result
pattern QueryAnytimeShelley q = QueryAnytime q (EraIndex (Z (K ())))

-- | Query about the Prototype One era that can be answered anytime, i.e.,
-- independent from where the tip of the ledger is.
--
-- For example, to ask for the start of the VoltairePrototype One era (whether the tip of the
-- ledger is in the VoltairePrototype One, VoltairePrototype Two, ... era), use:
--
-- > QueryAnytimeShelley EraStart
--
pattern QueryAnytimeVoltairePrototypeOne
  :: QueryAnytime result
  -> VoltairePrototypeQuery c result
pattern QueryAnytimeVoltairePrototypeOne q = QueryAnytime q (EraIndex (S (Z (K ()))))

pattern QueryAnytimeVoltairePrototypeTwo
  :: QueryAnytime result
  -> VoltairePrototypeQuery c result
pattern QueryAnytimeVoltairePrototypeTwo q = QueryAnytime q (EraIndex (S (S (Z (K ())))))

{-# COMPLETE QueryIfCurrentShelley
           , QueryIfCurrentVoltairePrototypeOne
           , QueryIfCurrentVoltairePrototypeTwo
           , QueryAnytimeShelley
           , QueryAnytimeVoltairePrototypeOne
           , QueryAnytimeVoltairePrototypeTwo
           , QueryHardFork #-}

-- | The result of a 'VoltairePrototypeQuery'
--
-- Thanks to the pattern synonyms, you can treat this as a sum type with
-- constructors 'QueryResultSuccess' and 'QueryResultEraMismatch'.
type VoltairePrototypeQueryResult c = HardForkQueryResult (VoltairePrototypeEras c)

pattern QueryResultSuccess :: result -> VoltairePrototypeQueryResult c result
pattern QueryResultSuccess result = Right result

-- | A query from a different era than the ledger's era was sent.
pattern QueryResultEraMismatch :: EraMismatch -> VoltairePrototypeQueryResult c result
pattern QueryResultEraMismatch eraMismatch <- Left (mkEraMismatch -> eraMismatch)

{-# COMPLETE QueryResultSuccess, QueryResultEraMismatch #-}

{-------------------------------------------------------------------------------
  CodecConfig
-------------------------------------------------------------------------------}

-- | The 'CodecConfig' for 'VoltairePrototypeBlock'.
--
-- Thanks to the pattern synonyms, you can treat this as the product of
-- the Shelley, VoltairePrototype ... 'CodecConfig's.
type VoltairePrototypeCodecConfig c = CodecConfig (VoltairePrototypeBlock c)

pattern VoltairePrototypeCodecConfig
  :: CodecConfig (ShelleyBlock (ShelleyEra c))
  -> CodecConfig (ShelleyBlock (VoltairePrototypeEra 'VoltairePrototype_One c))
  -> CodecConfig (ShelleyBlock (VoltairePrototypeEra 'VoltairePrototype_Two c))
  -> VoltairePrototypeCodecConfig c
pattern VoltairePrototypeCodecConfig cfgShelley cfgVoltairePrototypeOne cfgVoltairePrototypeTwo =
    HardForkCodecConfig {
        hardForkCodecConfigPerEra = PerEraCodecConfig
          (  cfgShelley
          :* cfgVoltairePrototypeOne
          :* cfgVoltairePrototypeTwo
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
type VoltairePrototypeBlockConfig c = BlockConfig (VoltairePrototypeBlock c)

pattern VoltairePrototypeBlockConfig
  :: BlockConfig (ShelleyBlock (ShelleyEra c))
  -> BlockConfig (ShelleyBlock (VoltairePrototypeEra 'VoltairePrototype_One c))
  -> BlockConfig (ShelleyBlock (VoltairePrototypeEra 'VoltairePrototype_Two c))
  -> VoltairePrototypeBlockConfig c
pattern VoltairePrototypeBlockConfig cfgShelley cfgVoltairePrototypeOne cfgVoltairePrototypeTwo =
    HardForkBlockConfig {
        hardForkBlockConfigPerEra = PerEraBlockConfig
          (  cfgShelley
          :* cfgVoltairePrototypeOne
          :* cfgVoltairePrototypeTwo
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
type VoltairePrototypeStorageConfig c = StorageConfig (VoltairePrototypeBlock c)

pattern VoltairePrototypeStorageConfig
  :: StorageConfig (ShelleyBlock (ShelleyEra c))
  -> StorageConfig (ShelleyBlock (VoltairePrototypeEra 'VoltairePrototype_One c))
  -> StorageConfig (ShelleyBlock (VoltairePrototypeEra 'VoltairePrototype_Two c))
  -> VoltairePrototypeStorageConfig c
pattern VoltairePrototypeStorageConfig cfgShelley cfgVoltairePrototypeOne cfgVoltairePrototypeTwo =
    HardForkStorageConfig {
        hardForkStorageConfigPerEra = PerEraStorageConfig
          (  cfgShelley
          :* cfgVoltairePrototypeOne
          :* cfgVoltairePrototypeTwo
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
type VoltairePrototypeConsensusConfig c =
  ConsensusConfig (HardForkProtocol (VoltairePrototypeEras c))

pattern VoltairePrototypeConsensusConfig
  :: PartialConsensusConfig (BlockProtocol (ShelleyBlock (ShelleyEra c)))
  -> PartialConsensusConfig (BlockProtocol (ShelleyBlock (VoltairePrototypeEra 'VoltairePrototype_One c)))
  -> PartialConsensusConfig (BlockProtocol (ShelleyBlock (VoltairePrototypeEra 'VoltairePrototype_Two c)))
  -> VoltairePrototypeConsensusConfig c
pattern VoltairePrototypeConsensusConfig cfgShelley cfgVoltairePrototypeOne cfgVoltairePrototypeTwo <-
    HardForkConsensusConfig {
        hardForkConsensusConfigPerEra = PerEraConsensusConfig
          (  WrapPartialConsensusConfig cfgShelley
          :* WrapPartialConsensusConfig cfgVoltairePrototypeOne
          :* WrapPartialConsensusConfig cfgVoltairePrototypeTwo
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
type VoltairePrototypeLedgerConfig c = HardForkLedgerConfig (VoltairePrototypeEras c)

pattern VoltairePrototypeLedgerConfig
  :: PartialLedgerConfig (ShelleyBlock (ShelleyEra c))
  -> PartialLedgerConfig (ShelleyBlock (VoltairePrototypeEra 'VoltairePrototype_One c))
  -> PartialLedgerConfig (ShelleyBlock (VoltairePrototypeEra 'VoltairePrototype_Two c))
  -> VoltairePrototypeLedgerConfig c
pattern VoltairePrototypeLedgerConfig cfgShelley cfgVoltairePrototypeOne cfgVoltairePrototypeTwo <-
    HardForkLedgerConfig {
        hardForkLedgerConfigPerEra = PerEraLedgerConfig
          ( WrapPartialLedgerConfig cfgShelley
          :* WrapPartialLedgerConfig cfgVoltairePrototypeOne
          :* WrapPartialLedgerConfig cfgVoltairePrototypeTwo
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
type VoltairePrototypeLedgerState c = LedgerState (VoltairePrototypeBlock c)

pattern LedgerStateShelley
  :: LedgerState (ShelleyBlock (ShelleyEra c))
  -> VoltairePrototypeLedgerState c
pattern LedgerStateShelley st <-
    HardForkLedgerState
      (State.HardForkState
        (TZ (State.Current { currentState = st })))

pattern LedgerStateVoltairePrototypeOne
  :: LedgerState (ShelleyBlock (VoltairePrototypeEra 'VoltairePrototype_One c))
  -> VoltairePrototypeLedgerState c
pattern LedgerStateVoltairePrototypeOne st <-
    HardForkLedgerState
      (State.HardForkState
        (TS _ (TZ (State.Current { currentState = st }))))

pattern LedgerStateVoltairePrototypeTwo
  :: LedgerState (ShelleyBlock (VoltairePrototypeEra 'VoltairePrototype_Two c))
  -> VoltairePrototypeLedgerState c
pattern LedgerStateVoltairePrototypeTwo st <-
    HardForkLedgerState
      (State.HardForkState
        (TS _ (TS _ (TZ (State.Current { currentState = st })))))

{-# COMPLETE LedgerStateShelley
           , LedgerStateVoltairePrototypeOne
           , LedgerStateVoltairePrototypeTwo #-}

{-------------------------------------------------------------------------------
  ChainDepState
-------------------------------------------------------------------------------}

-- | The 'ChainDepState' for 'VoltairePrototypeBlock'.
--
-- NOTE: the 'VoltairePrototypeChainDepState' contains more than just the current era's
-- 'ChainDepState'. We don't give access to those internal details through the
-- pattern synonyms. This is also the reason the pattern synonyms are not
-- bidirectional.
type VoltairePrototypeChainDepState c = HardForkChainDepState (VoltairePrototypeEras c)

pattern ChainDepStateShelley
  :: ChainDepState (BlockProtocol (ShelleyBlock (ShelleyEra c)))
  -> VoltairePrototypeChainDepState c
pattern ChainDepStateShelley st <-
    State.HardForkState
      (TZ (State.Current { currentState = WrapChainDepState st }))

pattern ChainDepStateVoltairePrototypeOne
  :: ChainDepState (BlockProtocol (ShelleyBlock (VoltairePrototypeEra 'VoltairePrototype_One c)))
  -> VoltairePrototypeChainDepState c
pattern ChainDepStateVoltairePrototypeOne st <-
    State.HardForkState
      (TS _ (TZ (State.Current { currentState = WrapChainDepState st })))

pattern ChainDepStateVoltairePrototypeTwo
  :: ChainDepState (BlockProtocol (ShelleyBlock (VoltairePrototypeEra 'VoltairePrototype_Two c)))
  -> VoltairePrototypeChainDepState c
pattern ChainDepStateVoltairePrototypeTwo st <-
    State.HardForkState
      (TS _ (TS _ (TZ (State.Current { currentState = WrapChainDepState st }))))

{-# COMPLETE ChainDepStateShelley
           , ChainDepStateVoltairePrototypeOne
           , ChainDepStateVoltairePrototypeTwo #-}
