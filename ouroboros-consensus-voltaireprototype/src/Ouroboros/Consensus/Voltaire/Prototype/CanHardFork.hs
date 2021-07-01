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

import qualified Cardano.Ledger.Era as SL
import           Cardano.Ledger.Tx (Tx)

import           Ouroboros.Consensus.Voltaire.Prototype.Block
import           Cardano.Ledger.Voltaire.Prototype.Class
import           Shelley.Spec.Ledger.LedgerState (NewEpochState)
import Ouroboros.Consensus.Shelley.Update (HasProtocolUpdates)

{-------------------------------------------------------------------------------
  CanHardFork
-------------------------------------------------------------------------------}

type VoltairePrototypeHardForkConstraints proto c =
  ( PraosCrypto c
  , VoltaireClass (VoltairePrototypeEra proto c)
  , HasProtocolUpdates (VoltairePrototypeEra proto c)
  , ShelleyBasedEra (ShelleyEra c)
  , ShelleyBasedEra (VoltairePrototypeEra proto c)
  , SL.TranslationError (VoltairePrototypeEra proto c) NewEpochState ~ Void
  , SL.TranslateEra (VoltairePrototypeEra proto c) NewEpochState
  , SL.TranslationContext (VoltairePrototypeEra proto c) ~ ()
  , SL.TranslateEra (VoltairePrototypeEra proto c) Tx
  , SL.TranslationError (VoltairePrototypeEra proto c) ShelleyGenesis ~ Void
  , SL.TranslateEra (VoltairePrototypeEra proto c) ShelleyGenesis
  )

instance ( VoltairePrototypeHardForkConstraints 'VoltairePrototype_One c
         , VoltairePrototypeHardForkConstraints 'VoltairePrototype_Two c
         )
         => CanHardFork (VoltairePrototypeEras c) where
  hardForkEraTranslation = EraTranslation {
      translateLedgerState   =
          PCons translateLedgerStateVoltaireWrapper
        $ PCons translateLedgerStateVoltaireWrapper
        $ PNil
    , translateChainDepState =
          PCons translateChainDepStateAcrossShelley
        $ PCons translateChainDepStateAcrossShelley
        $ PNil
    , translateLedgerView    =
          PCons translateLedgerViewAcrossShelley
        $ PCons translateLedgerViewAcrossShelley
        $ PNil
    }

  hardForkChainSel =
        TCons (SelectSameProtocol :* SelectSameProtocol :* Nil)
      $ TCons (SelectSameProtocol :* Nil)
      $ TCons Nil
      $ TNil
  hardForkInjectTxs =
        PCons (ignoringBoth translateTxVoltaireWrapper)
      $ PCons (ignoringBoth translateTxVoltaireWrapper)
      $ PNil
    where

translateLedgerStateVoltaireWrapper ::
  ( ShelleyBasedEra era
  , ShelleyBasedEra (SL.PreviousEra era)
  , SL.TranslateEra era NewEpochState
  , SL.TranslationError era NewEpochState ~ Void
  , SL.Crypto (SL.PreviousEra era) ~ SL.Crypto era
  , SL.TranslationContext era ~ ()
  )
  => RequiringBoth
       WrapLedgerConfig
       (Translate LedgerState)
       (ShelleyBlock (SL.PreviousEra era))
       (ShelleyBlock era)
translateLedgerStateVoltaireWrapper =
    ignoringBoth $
      Translate $ \_epochNo ->
        unComp . SL.translateEra' () . Comp

translateTxVoltaireWrapper ::
      ( ShelleyBasedEra era
      , SL.TranslationContext era ~ ()
      , SL.TranslateEra era Tx
      )
  => InjectTx
       (ShelleyBlock (SL.PreviousEra era))
       (ShelleyBlock era)
translateTxVoltaireWrapper = InjectTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra () . Comp
