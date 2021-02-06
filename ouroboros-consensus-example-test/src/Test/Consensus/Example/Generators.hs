{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | 'Arbitrary' instances intended for serialisation roundtrip tests for
-- 'ExampleBlock' and its related types.
--
-- Because the generated values are only used in serialisation roundtrip tests,
-- they don't need to be valid blocks, transactions, etc.
--
-- We combine the Shelley-based instances defined elsewhere into
-- Example consensus instances by picking randomly from one of the eras.
module Test.Consensus.Example.Generators (
  ) where

import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Proxy
import           Data.SOP.Strict

import           Test.QuickCheck

import           Ouroboros.Consensus.Block
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Serialisation (Some (..))
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util.Counting (NonEmpty (..),
                     nonEmptyFromList, nonEmptyToList)
import           Ouroboros.Consensus.Util.SOP (nsFromIndex)

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation

import           Ouroboros.Consensus.Shelley.Ledger

import           Ouroboros.Consensus.Example.Block
import           Ouroboros.Consensus.Example.Node (ExampleHardForkConstraints)

import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Serialisation.Roundtrip (WithVersion (..))

import           Test.Cardano.Ledger.Example
import           Test.Consensus.Shelley.Generators
import           Test.Consensus.Shelley.MockCrypto (CanMock)

import           Test.Consensus.Cardano.MockCrypto

{-------------------------------------------------------------------------------
  Disk
-------------------------------------------------------------------------------}

instance Arbitrary (ExampleBlock MockCryptoCompatByron) where
  arbitrary = HardForkBlock . OneEraBlock <$> arbitrary

instance Arbitrary (ExampleHeader MockCryptoCompatByron) where
  arbitrary = getHeader <$> arbitrary

instance (CanMock (ShelleyEra c), ExampleHardForkConstraints c)
      => Arbitrary (OneEraHash (ExampleEras c)) where
  arbitrary = inj <$> arbitrary
    where
      inj :: NS WrapHeaderHash (ExampleEras c) -> OneEraHash (ExampleEras c)
      inj = hcollapse . hcmap proxySingle aux

      aux ::
           forall blk. SingleEraBlock blk
        => WrapHeaderHash blk -> K (OneEraHash (ExampleEras c)) blk
      aux = K . OneEraHash . toShortRawHash (Proxy @blk) . unwrapHeaderHash

instance (c ~ MockCryptoCompatByron, ShelleyBasedEra (ShelleyEra c))
      => Arbitrary (AnnTip (ExampleBlock c)) where
  arbitrary = AnnTip
      <$> (SlotNo <$> arbitrary)
      <*> arbitrary
      <*> (OneEraTipInfo <$> arbitrary)

{-------------------------------------------------------------------------------
  NodeToNode
-------------------------------------------------------------------------------}

instance ExampleHardForkConstraints c
      => Arbitrary (HardForkNodeToNodeVersion (ExampleEras c)) where
  arbitrary =
    elements $ Map.elems $ supportedNodeToNodeVersions (Proxy @(ExampleBlock c))

instance Arbitrary (BlockNodeToNodeVersion blk)
     => Arbitrary (EraNodeToNodeVersion blk) where
  arbitrary = frequency
    [ (1, pure EraNodeToNodeDisabled)
    , (9, EraNodeToNodeEnabled <$> arbitrary)
    ]

arbitraryNodeToNode
  :: ( Arbitrary shelley
     , Arbitrary example
     )
  => (shelley -> consensus)
  -> (example -> consensus)
  -> Gen (WithVersion (HardForkNodeToNodeVersion (ExampleEras c)) consensus)
arbitraryNodeToNode injShelley injExample = oneof
    -- Shelley + HardFork enabled
    [ (\(WithVersion versionShelley s) versionExample ->
          WithVersion
            (HardForkNodeToNodeEnabled
              maxBound
              (  EraNodeToNodeEnabled versionShelley
              :* versionExample
              :* Nil
              ))
            (injShelley s))
        <$> arbitrary <*> arbitrary
    -- Example + HardFork enabled
    , (\versionShelley (WithVersion versionExample a) ->
          WithVersion
            (HardForkNodeToNodeEnabled
              maxBound
              (  EraNodeToNodeEnabled versionShelley
              :* EraNodeToNodeEnabled versionExample
              :* Nil
              ))
            (injExample a))
        <$> arbitrary <*> arbitrary
    ]

instance c ~ MockCryptoCompatByron
      => Arbitrary (WithVersion (HardForkNodeToNodeVersion (ExampleEras c))
                                (SomeSecond (NestedCtxt Header) (ExampleBlock c))) where
  arbitrary = arbitraryNodeToNode injShelley injExample
    where
      injShelley   = mapSomeNestedCtxt NCZ
      injExample = mapSomeNestedCtxt (NCS . NCZ)

instance c ~ MockCryptoCompatByron
      => Arbitrary (WithVersion (HardForkNodeToNodeVersion (ExampleEras c))
                                (ExampleBlock c)) where
  arbitrary = arbitraryNodeToNode BlockShelley BlockExample

instance c ~ MockCryptoCompatByron
      => Arbitrary (WithVersion (HardForkNodeToNodeVersion (ExampleEras c))
                                (ExampleHeader c)) where
  arbitrary = arbitraryNodeToNode HeaderShelley HeaderExample

instance c ~ MockCryptoCompatByron
      => Arbitrary (WithVersion (HardForkNodeToNodeVersion (ExampleEras c))
                                (ExampleGenTx c)) where
  arbitrary = arbitraryNodeToNode GenTxShelley GenTxExample

instance c ~ MockCryptoCompatByron
      => Arbitrary (WithVersion (HardForkNodeToNodeVersion (ExampleEras c))
                                (ExampleGenTxId c)) where
  arbitrary = arbitraryNodeToNode GenTxIdShelley GenTxIdExample

{-------------------------------------------------------------------------------
  NodeToClient
-------------------------------------------------------------------------------}

instance ExampleHardForkConstraints c
      => Arbitrary (HardForkNodeToClientVersion (ExampleEras c)) where
  arbitrary =
    elements $ Map.elems $ supportedNodeToClientVersions (Proxy @(ExampleBlock c))

newtype HardForkEnabledNodeToClientVersion c = HardForkEnabledNodeToClientVersion {
      getHardForkEnabledNodeToClientVersion :: HardForkNodeToClientVersion (ExampleEras c)
    }

deriving newtype instance ExampleHardForkConstraints c
                       => Eq (HardForkEnabledNodeToClientVersion c)
deriving newtype instance ExampleHardForkConstraints c
                       => Show (HardForkEnabledNodeToClientVersion c)

instance ExampleHardForkConstraints c
      => Arbitrary (HardForkEnabledNodeToClientVersion c) where
  arbitrary =
        elements
      . map HardForkEnabledNodeToClientVersion
      . filter isHardForkNodeToClientEnabled
      . Map.elems
      . supportedNodeToClientVersions
      $ Proxy @(ExampleBlock c)

-- | Generate a supported 'HardForkNodeToClientVersion' of which the
-- 'HardForkSpecificNodeToClientVersion' satisfies the given predicate.
--
-- PRECONDITION: 'supportedNodeToClientVersions' must include a version that
-- satisfies this condition.
genWithHardForkSpecificNodeToClientVersion ::
     forall c. ExampleHardForkConstraints c
  => (HardForkSpecificNodeToClientVersion -> Bool)
  -> Gen (HardForkNodeToClientVersion (ExampleEras c))
genWithHardForkSpecificNodeToClientVersion p =
      elements
    . filter p'
    . Map.elems
    . supportedNodeToClientVersions
    $ Proxy @(ExampleBlock c)
  where
    p' :: HardForkNodeToClientVersion (ExampleEras c) -> Bool
    p' (HardForkNodeToClientEnabled v _) = p v
    p' (HardForkNodeToClientDisabled {}) = False

instance Arbitrary (BlockNodeToClientVersion blk)
     => Arbitrary (EraNodeToClientVersion blk) where
  arbitrary = frequency
    [ (1, pure EraNodeToClientDisabled)
    , (9, EraNodeToClientEnabled <$> arbitrary)
    ]

arbitraryNodeToClient
  :: ( Arbitrary (WithVersion ShelleyNodeToClientVersion shelley)
     , Arbitrary (WithVersion ShelleyNodeToClientVersion example)
     )
  => (shelley -> consensus)
  -> (example -> consensus)
  -> Gen (WithVersion (HardForkNodeToClientVersion (ExampleEras c)) consensus)
arbitraryNodeToClient injShelley injExample = oneof
    -- Shelley + HardFork enabled
    [ (\(WithVersion versionShelley s) versionExample ->
          WithVersion
            (HardForkNodeToClientEnabled
              maxBound
              ( EraNodeToClientEnabled versionShelley
              :* versionExample
              :* Nil
              ))
            (injShelley s))
        <$> arbitrary <*> arbitrary
    -- Example + HardFork enabled
    , (\versionShelley (WithVersion versionExample a) ->
          WithVersion
            (HardForkNodeToClientEnabled
              maxBound
              ( EraNodeToClientEnabled versionShelley
              :* EraNodeToClientEnabled versionExample
              :* Nil
              ))
            (injExample a))
        <$> arbitrary <*> arbitrary
    ]

instance c ~ MockCryptoCompatByron
      => Arbitrary (WithVersion (HardForkNodeToClientVersion (ExampleEras c))
                                (ExampleBlock c)) where
  arbitrary = arbitraryNodeToClient BlockShelley BlockExample

instance c ~ MockCryptoCompatByron
      => Arbitrary (WithVersion (HardForkNodeToClientVersion (ExampleEras c))
                                (ExampleGenTx c)) where
  arbitrary = arbitraryNodeToClient GenTxShelley GenTxExample

instance c ~ MockCryptoCompatByron
      => Arbitrary (WithVersion (HardForkNodeToClientVersion (ExampleEras c))
                                (ExampleApplyTxErr c)) where
  arbitrary = frequency
      [ (8, arbitraryNodeToClient ApplyTxErrShelley ApplyTxErrExample)
      , (2, WithVersion
              <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
              <*> (HardForkApplyTxErrWrongEra <$> arbitrary))
      ]
  shrink = traverse aux
    where
      aux :: ExampleApplyTxErr MockCryptoCompatByron
         -> [ExampleApplyTxErr MockCryptoCompatByron]
      aux (HardForkApplyTxErrFromEra (OneEraApplyTxErr x)) =
          HardForkApplyTxErrFromEra . OneEraApplyTxErr <$> shrink x
      aux (HardForkApplyTxErrWrongEra x) =
          HardForkApplyTxErrWrongEra <$> shrink x

instance Arbitrary (Some QueryAnytime) where
  arbitrary = return $ Some GetEraStart

instance ExampleHardForkConstraints c
      => Arbitrary (WithVersion (HardForkNodeToClientVersion (ExampleEras c))
                                (Some (QueryHardFork (ExampleEras c)))) where
  arbitrary = frequency
      [ (1, do version <- getHardForkEnabledNodeToClientVersion <$> arbitrary
               return $ WithVersion version (Some GetInterpreter))
      , (1, do version <- genWithHardForkSpecificNodeToClientVersion
                            (>= HardForkSpecificNodeToClientVersion2)
               return $ WithVersion version (Some GetCurrentEra))
      ]

instance c ~ MockCryptoCompatByron
      => Arbitrary (WithVersion (HardForkNodeToClientVersion (ExampleEras c))
                                (SomeSecond Query (ExampleBlock c))) where
  arbitrary = frequency
      [ (1, arbitraryNodeToClient injShelley injExample)
      , (1, WithVersion
              <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
              <*> (injAnytimeShelley <$> arbitrary))
      , (1, WithVersion
              <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
              <*> (injAnytimeExample <$> arbitrary))
      , (1, fmap injHardFork <$> arbitrary)
      ]
    where
      injShelley        (SomeSecond query) = SomeSecond (QueryIfCurrentShelley query)
      injExample        (SomeSecond query) = SomeSecond (QueryIfCurrentExample query)
      injAnytimeShelley (Some      query)  = SomeSecond (QueryAnytimeShelley   query)
      injAnytimeExample (Some      query)  = SomeSecond (QueryAnytimeExample   query)
      injHardFork       (Some      query)  = SomeSecond (QueryHardFork         query)

instance Arbitrary History.EraEnd where
  arbitrary = oneof
      [ History.EraEnd <$> arbitrary
      , return History.EraUnbounded
      ]

instance Arbitrary History.SafeZone where
  arbitrary = oneof
      [ History.StandardSafeZone <$> arbitrary
      , return History.UnsafeIndefiniteSafeZone
      ]

instance Arbitrary History.EraParams where
  arbitrary = History.EraParams
      <$> (EpochSize <$> arbitrary)
      <*> arbitrary
      <*> arbitrary

instance Arbitrary History.EraSummary where
  arbitrary = History.EraSummary
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary

instance (Arbitrary a, SListI xs) => Arbitrary (NonEmpty xs a) where
  arbitrary = do
      let nbXs = lengthSList (Proxy @xs)
      len <- choose (1, nbXs)
      xs  <- vectorOf len arbitrary
      return $ fromMaybe (error "nonEmptyFromList failed") $ nonEmptyFromList xs

instance Arbitrary (History.Interpreter (ExampleEras c)) where
  arbitrary =
      History.mkInterpreter . History.Summary . enforceInvariant <$> arbitrary
    where
      -- Enforce the invariant that when the last era in the summary is the
      -- final era, it is unbounded. The decoder relies on this.
      enforceInvariant xs
        | length (nonEmptyToList xs) == lengthSList (Proxy @(ExampleEras c))
        = fixEndBound xs
        | otherwise
        = xs

      fixEndBound ::
           NonEmpty xs History.EraSummary
        -> NonEmpty xs History.EraSummary
      fixEndBound (NonEmptyCons e es) = NonEmptyCons e (fixEndBound es)
      fixEndBound (NonEmptyOne  e)    =
          NonEmptyOne  e { History.eraEnd = History.EraUnbounded }

instance Arbitrary (EraIndex (ExampleEras c)) where
  arbitrary = do
    let nbEras = lengthSList (Proxy @(ExampleEras c))
    index <- choose (0, fromIntegral nbEras - 1)
    case nsFromIndex index of
      Nothing -> error $ "nsFromIndex failed for " <> show index
      Just ns -> return $ eraIndexFromNS ns

instance c ~ MockCryptoCompatByron
      => Arbitrary (WithVersion (HardForkNodeToClientVersion (ExampleEras c))
                                (SomeResult (ExampleBlock c))) where
  arbitrary = frequency
      [ (1, arbitraryNodeToClient injShelley injExample)
      , (1, WithVersion
              <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
              <*> genQueryIfCurrentResultEraMismatch)
      , (1, WithVersion
              <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
              <*> genQueryAnytimeResultShelley)
      , (1, WithVersion
              <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
              <*> genQueryAnytimeResultExample)
      , (1, WithVersion
              <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
              <*> genQueryHardForkResult)
      ]
    where
      injShelley (SomeResult q r) = SomeResult (QueryIfCurrentShelley q) (QueryResultSuccess r)
      injExample (SomeResult q r) = SomeResult (QueryIfCurrentExample q) (QueryResultSuccess r)

      -- In practice, when sending a Byron query you'll never get a mismatch
      -- saying that your query is from the Shelley era while the ledger is
      -- from Byron. Only the inverse. We ignore that in this generator, as it
      -- doesn't matter for serialisation purposes, we just generate a random
      -- 'MismatchEraInfo'.
      genQueryIfCurrentResultEraMismatch :: Gen (SomeResult (ExampleBlock c))
      genQueryIfCurrentResultEraMismatch = oneof
          [ (\(SomeResult q (_ :: result)) mismatch ->
                SomeResult (QueryIfCurrentShelley q) (Left @_ @result mismatch))
              <$> arbitrary <*> arbitrary
          , (\(SomeResult q (_ :: result)) mismatch ->
                SomeResult (QueryIfCurrentExample q) (Left @_ @result mismatch))
              <$> arbitrary <*> arbitrary
          ]

      genQueryAnytimeResultShelley :: Gen (SomeResult (ExampleBlock c))
      genQueryAnytimeResultShelley =
          SomeResult (QueryAnytimeShelley GetEraStart) <$> arbitrary

      genQueryAnytimeResultExample :: Gen (SomeResult (ExampleBlock c))
      genQueryAnytimeResultExample =
          SomeResult (QueryAnytimeExample GetEraStart) <$> arbitrary

      genQueryHardForkResult :: Gen (SomeResult (ExampleBlock c))
      genQueryHardForkResult = oneof
          [ SomeResult (QueryHardFork GetInterpreter) <$> arbitrary
          , SomeResult (QueryHardFork GetCurrentEra)  <$> arbitrary
          ]
