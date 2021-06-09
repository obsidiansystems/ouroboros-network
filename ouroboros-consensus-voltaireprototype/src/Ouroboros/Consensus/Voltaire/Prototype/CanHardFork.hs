{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TupleSections            #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Voltaire.Prototype.CanHardFork (
    VoltairePrototypeHardForkConstraints
  , ShelleyPartialLedgerConfig (..)
  , TriggerHardFork (..)
  , forecastAcrossShelley
  , translateChainDepStateAcrossShelley
  ) where

import           Control.Monad.Except (runExcept)
import           Data.SOP.Strict (NP (..), unComp, (:.:) (..))
import           Data.Void (Void)

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util (eitherToMaybe)

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.State.Types
import           Ouroboros.Consensus.HardFork.Combinator.Util.InPairs
                     (RequiringBoth (..), ignoringBoth)
import           Ouroboros.Consensus.HardFork.Combinator.Util.Tails (Tails (..))
import           Ouroboros.Consensus.HardFork.Simple

import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Node ()
import           Ouroboros.Consensus.Shelley.Protocol
import           Ouroboros.Consensus.Shelley.ShelleyHFC
import           Ouroboros.Consensus.Shelley.Eras (WithShelleyUpdates)

import qualified Cardano.Ledger.Era as SL
import           Cardano.Ledger.Tx (Tx)

import           Ouroboros.Consensus.Voltaire.Prototype.Block
import           Cardano.Ledger.Voltaire.Prototype (VoltairePrototype(VoltairePrototype_One))
import           Shelley.Spec.Ledger.LedgerState (NewEpochState)

{-------------------------------------------------------------------------------
  CanHardFork
-------------------------------------------------------------------------------}

type VoltairePrototypeHardForkConstraints c =
  ( PraosCrypto c
  , ShelleyBasedEra (ShelleyEra c)
  , ShelleyBasedEra (VoltairePrototypeEra 'VoltairePrototype_One c)
  , SL.PreviousEra (VoltairePrototypeEra 'VoltairePrototype_One c) ~ ShelleyEra c
  , SL.TranslationError (VoltairePrototypeEra 'VoltairePrototype_One c) NewEpochState ~ Void
  , SL.TranslateEra (VoltairePrototypeEra 'VoltairePrototype_One c) NewEpochState
  , SL.TranslationContext (VoltairePrototypeEra 'VoltairePrototype_One c) ~ ()
  , SL.TranslateEra (VoltairePrototypeEra 'VoltairePrototype_One c) Tx
  )

instance PraosCrypto c => WithShelleyUpdates (VoltairePrototypeEra 'VoltairePrototype_One c)

instance VoltairePrototypeHardForkConstraints c => CanHardFork (VoltairePrototypeEras c) where
  hardForkEraTranslation = EraTranslation {
      translateLedgerState   =
          PCons translateLedgerStateShelleyToVoltairePrototypeWrapper
        $ PNil
    , translateChainDepState =
          PCons translateChainDepStateAcrossShelley
        $ PNil
    , translateLedgerView    =
          PCons translateLedgerViewAcrossShelley
        $ PNil
    }
  hardForkChainSel =
        -- Shelley <-> VoltairePrototype, ...
        TCons (SelectSameProtocol :* Nil)
        -- VoltairePrototype <-> ...
      $ TCons Nil
      $ TNil
  hardForkInjectTxs =
        PCons (ignoringBoth translateTxShelleyToVoltairePrototypeWrapper)
      $ PNil

{-------------------------------------------------------------------------------
  Translation from Shelley to VoltairePrototype
-------------------------------------------------------------------------------}

translateLedgerStateShelleyToVoltairePrototypeWrapper ::
     ( PraosCrypto c
     , SL.TranslationError (VoltairePrototypeEra 'VoltairePrototype_One c) NewEpochState ~ Void
     , SL.TranslateEra (VoltairePrototypeEra 'VoltairePrototype_One c) NewEpochState
     , SL.TranslationContext (VoltairePrototypeEra 'VoltairePrototype_One c) ~ ()
     )
  => RequiringBoth
       WrapLedgerConfig
       (Translate LedgerState)
       (ShelleyBlock (ShelleyEra c))
       (ShelleyBlock (VoltairePrototypeEra 'VoltairePrototype_One c))
translateLedgerStateShelleyToVoltairePrototypeWrapper =
    ignoringBoth $
      Translate $ \_epochNo ->
        unComp . SL.translateEra' () . Comp

translateTxShelleyToVoltairePrototypeWrapper ::
     ( PraosCrypto c
     , SL.TranslateEra (VoltairePrototypeEra 'VoltairePrototype_One c) Tx
     , SL.TranslationContext (VoltairePrototypeEra 'VoltairePrototype_One c) ~ ()
     )
  => InjectTx
       (ShelleyBlock (ShelleyEra c))
       (ShelleyBlock (VoltairePrototypeEra 'VoltairePrototype_One c))
translateTxShelleyToVoltairePrototypeWrapper = InjectTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra () . Comp