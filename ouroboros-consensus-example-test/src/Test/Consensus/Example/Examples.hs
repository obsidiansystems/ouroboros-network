{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Example.Examples (
    -- * Setup
    codecConfig
    -- * Examples
  , examples
  , exampleQueryAnytimeShelley
  , exampleResultAnytimeShelley
  ) where

import           Data.Coerce (Coercible)
import           Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import           Data.SOP.Strict

import           Ouroboros.Network.Block (Serialised (..))

import           Ouroboros.Consensus.Block
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HeaderValidation (AnnTip)
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr)
import           Ouroboros.Consensus.Storage.Serialisation
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util.Counting (Exactly (..))
import           Ouroboros.Consensus.Util.SOP (Index (..))

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Embed.Nary
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State

import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger as Shelley

import           Ouroboros.Consensus.Example.Block
import           Ouroboros.Consensus.Example.CanHardFork ()

import qualified Cardano.Ledger.AuxiliaryData as SL (AuxiliaryDataHash (..))
import qualified Cardano.Ledger.Core as Core
import           Cardano.Ledger.Crypto (ADDRHASH, Crypto, DSIGN, HASH, VRF)
import           Shelley.Spec.Ledger.API (StrictMaybe (..))
import qualified Shelley.Spec.Ledger.API as SL
import qualified Shelley.Spec.Ledger.BaseTypes as SL (Seed (..),
                     mkNonceFromNumber, textToUrl)
import qualified Shelley.Spec.Ledger.BlockChain as SL (TxSeq (..))
import qualified Shelley.Spec.Ledger.Coin as SL (DeltaCoin (..))
import qualified Shelley.Spec.Ledger.Delegation.Certificates as SL
                     (IndividualPoolStake (..))
import qualified Shelley.Spec.Ledger.EpochBoundary as SL (BlocksMade (..),
                     emptySnapShots)
import qualified Shelley.Spec.Ledger.Hashing as SL (hashAnnotated)
import qualified Shelley.Spec.Ledger.Keys as SL (asWitness, hashWithSerialiser,
                     signedKES)
import qualified Shelley.Spec.Ledger.PParams as SL (emptyPParams,
                     emptyPParamsUpdate)
import qualified Shelley.Spec.Ledger.STS.Delegs as SL
                     (DelegsPredicateFailure (..))
import qualified Shelley.Spec.Ledger.STS.Ledger as SL
                     (LedgerPredicateFailure (..))
import qualified Shelley.Spec.Ledger.STS.Ledgers as SL
                     (LedgersPredicateFailure (..))
import qualified Shelley.Spec.Ledger.Tx as SL (addrWits)
import qualified Shelley.Spec.Ledger.UTxO as SL (makeWitnessesVKey)
import qualified Test.Shelley.Spec.Ledger.Generator.Core as SL
                     (AllIssuerKeys (..), mkOCert)
import           Test.Shelley.Spec.Ledger.Orphans ()
import qualified Test.Shelley.Spec.Ledger.Utils as SL hiding (mkKeyPair,
                     mkKeyPair', mkVRFKeyPair)

import           Test.Util.Serialisation.Golden (Examples, Labelled, labelled)
import qualified Test.Util.Serialisation.Golden as Golden
import           Test.Util.Serialisation.Roundtrip (SomeResult (..))

import qualified Test.Consensus.Shelley.Examples as Shelley

eraExamples :: NP Examples (ExampleEras StandardCrypto)
eraExamples =
       Shelley.examplesShelley
    :* examplesExample
    :* Nil

examplesExample :: Golden.Examples (ShelleyBlock StandardExample)
examplesExample = Shelley.examples
  Shelley.exampleCoin
  exampleTxBodyExample
  exampleAuxiliaryDataExample

exampleTxBodyExample :: SL.TxBody StandardExample
exampleTxBodyExample = SL.TxBody
    Shelley.exampleTxIns
    (StrictSeq.fromList [
        SL.TxOut (SL.mkAddr (Shelley.examplePayKey, Shelley.exampleStakeKey)) (SL.Coin 100000)
      ])
    Shelley.exampleCerts
    Shelley.exampleWithdrawals
    (SL.Coin 3)
    (SlotNo 10)
    (SJust (SL.Update Shelley.exampleProposedPPUpdates (EpochNo 0)))
    (SJust auxiliaryDataHash)
  where
    -- Dummy hash to decouple from the auxiliaryData in 'exampleTx'.
    auxiliaryDataHash :: SL.AuxiliaryDataHash StandardCrypto
    auxiliaryDataHash =
        SL.AuxiliaryDataHash $ Shelley.mkDummyHash (Proxy @(HASH StandardCrypto)) 30

exampleAuxiliaryDataExample :: Core.AuxiliaryData StandardExample
exampleAuxiliaryDataExample = SL.Metadata Shelley.exampleMetadataMap

-- | By using this function, we can't forget to update this test when adding a
-- new era to 'ExampleEras'.
combineEras ::
     NP Examples (ExampleEras StandardCrypto)
  -> Examples (ExampleBlock StandardCrypto)
combineEras = mconcat . hcollapse . hap eraInjections
  where
    eraInjections :: NP (Examples -.-> K (Examples (ExampleBlock StandardCrypto)))
                        (ExampleEras StandardCrypto)
    eraInjections =
           fn (K . injExamples "Shelley" IZ)
        :* fn (K . injExamples "Example" (IS IZ))
        :* Nil

    injExamples ::
         String
      -> Index (ExampleEras StandardCrypto) blk
      -> Examples blk
      -> Examples (ExampleBlock StandardCrypto)
    injExamples eraName idx =
          Golden.prefixExamples eraName
        . inject exampleStartBounds idx

{-------------------------------------------------------------------------------
  Inject instances
-------------------------------------------------------------------------------}

-- | In reality, an era tag would be prepended, but we're testing that the
-- encoder doesn't care what the bytes are.
instance Inject Serialised where
  inject _ _ (Serialised _) = Serialised "<EXAMPLE_BLOCK>"

instance Inject SomeResult where
  inject _ idx (SomeResult q r) =
      SomeResult (QueryIfCurrent (injectQuery idx q)) (Right r)

instance Inject Examples where
  inject startBounds (idx :: Index xs x) Golden.Examples {..} = Golden.Examples {
        exampleBlock            = inj (Proxy @I)                  exampleBlock
      , exampleSerialisedBlock  = inj (Proxy @Serialised)         exampleSerialisedBlock
      , exampleHeader           = inj (Proxy @Header)             exampleHeader
      , exampleSerialisedHeader = inj (Proxy @SerialisedHeader)   exampleSerialisedHeader
      , exampleHeaderHash       = inj (Proxy @WrapHeaderHash)     exampleHeaderHash
      , exampleGenTx            = inj (Proxy @GenTx)              exampleGenTx
      , exampleGenTxId          = inj (Proxy @WrapGenTxId)        exampleGenTxId
      , exampleApplyTxErr       = inj (Proxy @WrapApplyTxErr)     exampleApplyTxErr
      , exampleQuery            = inj (Proxy @(SomeSecond Query)) exampleQuery
      , exampleResult           = inj (Proxy @SomeResult)         exampleResult
      , exampleAnnTip           = inj (Proxy @AnnTip)             exampleAnnTip
      , exampleLedgerState      = inj (Proxy @LedgerState)        exampleLedgerState
      , exampleChainDepState    = inj (Proxy @WrapChainDepState)  exampleChainDepState
      , exampleExtLedgerState   = inj (Proxy @ExtLedgerState)     exampleExtLedgerState
      }
    where
      inj ::
           forall f a b.
           ( Inject f
           , Coercible a (f x)
           , Coercible b (f (HardForkBlock xs))
           )
        => Proxy f -> Labelled a -> Labelled b
      inj p = fmap (fmap (inject' p startBounds idx))

{-------------------------------------------------------------------------------
  Setup
-------------------------------------------------------------------------------}

shelleyEraParams :: History.EraParams
shelleyEraParams = Shelley.shelleyEraParams Shelley.testShelleyGenesis

exampleEraParams :: History.EraParams
exampleEraParams = Shelley.shelleyEraParams Shelley.testShelleyGenesis

-- | We use 10, 20, 30, 40, ... as the transition epochs
exampleTransitionEpoch :: EpochNo
exampleTransitionEpoch = 10

shelleyStartBound :: History.Bound
shelleyStartBound = History.initBound

exampleStartBound :: History.Bound
exampleStartBound =
    History.mkUpperBound
      shelleyEraParams
      shelleyStartBound
      exampleTransitionEpoch

exampleStartBounds :: Exactly (ExampleEras StandardCrypto) History.Bound
exampleStartBounds = Exactly $
    ( K shelleyStartBound
    :* K exampleStartBound
    :* Nil
    )

exampleShape :: History.Shape (ExampleEras StandardCrypto)
exampleShape = History.Shape $ Exactly $
    (  K shelleyEraParams
    :* K exampleEraParams
    :* Nil
    )

summary :: History.Summary (ExampleEras StandardCrypto)
summary =
    State.reconstructSummary
      exampleShape
      (State.TransitionKnown exampleTransitionEpoch)
      (hardForkLedgerStatePerEra (ledgerStateShelley shelleyLedger))
  where
    (_, shelleyLedger) = head $ Golden.exampleLedgerState Shelley.examplesShelley

eraInfoShelley :: SingleEraInfo (ShelleyBlock StandardShelley)
eraInfoShelley = singleEraInfo (Proxy @(ShelleyBlock StandardShelley))

codecConfig :: ExampleCodecConfig StandardCrypto
codecConfig =
    ExampleCodecConfig
      Shelley.ShelleyCodecConfig
      Shelley.ShelleyCodecConfig

ledgerStateShelley ::
     LedgerState (ShelleyBlock (ShelleyEra StandardCrypto))
  -> LedgerState (ExampleBlock StandardCrypto)
ledgerStateShelley stShelley =
    HardForkLedgerState $ HardForkState $ TZ cur
  where
    cur = State.Current {
          currentStart = History.initBound
        , currentState = stShelley
        }

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

-- | Multi-era examples, e.g., applying a transaction to the wrong era.
multiEraExamples :: Examples (ExampleBlock StandardCrypto)
multiEraExamples = mempty {
      Golden.exampleApplyTxErr = labelled [
        ]
    , Golden.exampleQuery = labelled [
          ("AnytimeShelley", exampleQueryAnytimeShelley)
        , ("HardFork",       exampleQueryHardFork)
        ]
    , Golden.exampleResult = labelled [
          ("AnytimeShelley",     exampleResultAnytimeShelley)
        , ("HardFork",           exampleResultHardFork)
        ]
    }

-- | The examples: the examples from each individual era lifted in to
-- 'ExampleBlock' /and/ the multi-era examples.
examples :: Examples (ExampleBlock StandardCrypto)
examples = combineEras eraExamples <> multiEraExamples

exampleQueryAnytimeShelley :: SomeSecond Query (ExampleBlock StandardCrypto)
exampleQueryAnytimeShelley =
    SomeSecond (QueryAnytimeShelley GetEraStart)

exampleQueryHardFork :: SomeSecond Query (ExampleBlock StandardCrypto)
exampleQueryHardFork =
    SomeSecond (QueryHardFork GetInterpreter)

exampleResultAnytimeShelley :: SomeResult (ExampleBlock StandardCrypto)
exampleResultAnytimeShelley =
    SomeResult (QueryAnytimeShelley GetEraStart) (Just shelleyStartBound)

exampleResultHardFork :: SomeResult (ExampleBlock StandardCrypto)
exampleResultHardFork =
    SomeResult (QueryHardFork GetInterpreter) (History.mkInterpreter summary)
