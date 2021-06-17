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
import           Ouroboros.Consensus.Shelley.Node (ShelleyGenesis)
import           Ouroboros.Consensus.Shelley.Protocol
import           Ouroboros.Consensus.Shelley.ShelleyHFC
import           Ouroboros.Consensus.Shelley.Eras (WithShelleyUpdates)

import qualified Cardano.Ledger.Era as SL
import           Cardano.Ledger.Tx (Tx)

import           Ouroboros.Consensus.Voltaire.Prototype.Block
import           Cardano.Ledger.Voltaire.Prototype.Class
import           Cardano.Ledger.Voltaire.Prototype (VoltairePrototype(VoltairePrototype_One))
import           Shelley.Spec.Ledger.LedgerState (NewEpochState)

{-------------------------------------------------------------------------------
  CanHardFork
-------------------------------------------------------------------------------}

type VoltairePrototypeHardForkConstraints proto c =
  ( PraosCrypto c
  , VoltaireClass (VoltairePrototypeEra proto c)
  , ShelleyBasedEra (ShelleyEra c)
  , ShelleyBasedEra (VoltairePrototypeEra proto c)
  , SL.PreviousEra (VoltairePrototypeEra proto c) ~ ShelleyEra c
  , SL.TranslationError (VoltairePrototypeEra proto c) NewEpochState ~ Void
  , SL.TranslateEra (VoltairePrototypeEra proto c) NewEpochState
  , SL.TranslationContext (VoltairePrototypeEra proto c) ~ ()
  , SL.TranslateEra (VoltairePrototypeEra proto c) Tx
  , SL.TranslationError (VoltairePrototypeEra proto c) ShelleyGenesis ~ Void
  , SL.TranslateEra (VoltairePrototypeEra proto c) ShelleyGenesis
  )

instance WithShelleyUpdates (VoltairePrototypeEra 'VoltairePrototype_One c)

instance (VoltairePrototypeHardForkConstraints proto c, WithShelleyUpdates (VoltairePrototypeEra proto c)) => CanHardFork (VoltairePrototypeEras proto c) where
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
     ( SL.PreviousEra (VoltairePrototypeEra proto c) ~ ShelleyEra c
     , ShelleyBasedEra (VoltairePrototypeEra proto c)
     , SL.TranslationError (VoltairePrototypeEra proto c) NewEpochState ~ Void
     , SL.TranslateEra (VoltairePrototypeEra proto c) NewEpochState
     , SL.TranslationContext (VoltairePrototypeEra proto c) ~ ()
     )
  => RequiringBoth
       WrapLedgerConfig
       (Translate LedgerState)
       (ShelleyBlock (ShelleyEra c))
       (ShelleyBlock (VoltairePrototypeEra proto c))
translateLedgerStateShelleyToVoltairePrototypeWrapper =
    ignoringBoth $
      Translate $ \_epochNo ->
        unComp . SL.translateEra' () . Comp

translateTxShelleyToVoltairePrototypeWrapper ::
     ( SL.PreviousEra (VoltairePrototypeEra proto c) ~ ShelleyEra c
     , ShelleyBasedEra (VoltairePrototypeEra proto c)
     , SL.TranslateEra (VoltairePrototypeEra proto c) Tx
     , SL.TranslationContext (VoltairePrototypeEra proto c) ~ ()
     )
  => InjectTx
       (ShelleyBlock (ShelleyEra c))
       (ShelleyBlock (VoltairePrototypeEra proto c))
translateTxShelleyToVoltairePrototypeWrapper = InjectTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra () . Comp
