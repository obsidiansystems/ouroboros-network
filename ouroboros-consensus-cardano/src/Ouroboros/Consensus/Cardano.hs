{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Ouroboros.Consensus.Cardano (
    -- * The block type of the Cardano block chain
    CardanoBlock
    -- * Supported protocols
  , ProtocolByron
  , ProtocolShelley
  , ProtocolCardano
    -- * Abstract over the various protocols
  , ProtocolParamsByron(..)
  , ProtocolParamsShelley(..)
  , ProtocolParamsAllegra(..)
  , ProtocolParamsMary(..)
  , ProtocolParamsTransition(..)
  , Protocol(..)
  , module X
    -- * Client support for nodes running a protocol
  , ProtocolClient(..)
  , RunProtocolClient(..)
  ) where

import           Cardano.Chain.Slotting (EpochSlots)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Protocol.Abstract as X
import           Ouroboros.Consensus.Protocol.PBFT as X
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Embed.Unary

import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Byron.Node as X

import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Node as X

import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.ByronHFC
import           Ouroboros.Consensus.Cardano.Node
import           Ouroboros.Consensus.Cardano.ShelleyHFC

{-------------------------------------------------------------------------------
  Supported protocols

  We list these as explicit definitions here (rather than derived through
  'BlockProtocol'), and then /verify/ in 'verifyProtocol' that these definitions
  match. This provides an additional sanity check that we are not accidentally
  breaking any assumptions made in @cardano-node@.
-------------------------------------------------------------------------------}

type ProtocolByron   = HardForkProtocol '[ ByronBlock ]
type ProtocolShelley = HardForkProtocol '[ ShelleyBlock StandardShelley ]
type ProtocolCardano = HardForkProtocol '[ ByronBlock
                                         , ShelleyBlock StandardShelley
                                         , ShelleyBlock StandardAllegra
                                         , ShelleyBlock StandardMary
                                         ]

class (p ~ BlockProtocol blk, RunNode blk, IOLike m) => Protocol m blk p where
  data RunProtocol m blk p
  protocolInfo :: RunProtocol m blk p -> ProtocolInfo m blk

-- | Run PBFT against the Byron ledger
instance IOLike m => Protocol m ByronBlockHFC ProtocolByron where
  data RunProtocol m ByronBlockHFC ProtocolByron = RunProtocolByron ProtocolParamsByron
  protocolInfo (RunProtocolByron params) = inject $ protocolInfoByron params

-- | Run TPraos against the Shelley ledger
instance IOLike m => Protocol m (ShelleyBlockHFC StandardShelley) ProtocolShelley where
  data RunProtocol m (ShelleyBlockHFC StandardShelley) ProtocolShelley = RunProtocolShelley
    (ProtocolParamsShelleyBased StandardShelley)
    (ProtocolParamsShelley)
  protocolInfo (RunProtocolShelley paramsShelleyBased paramsShelley) =
    inject $ protocolInfoShelley paramsShelleyBased paramsShelley

instance IOLike m => Protocol m (CardanoBlock StandardCrypto) ProtocolCardano where
  data RunProtocol m (CardanoBlock StandardCrypto) ProtocolCardano = RunProtocolCardano
    ProtocolParamsByron
    (ProtocolParamsShelleyBased StandardShelley)
    ProtocolParamsShelley
    ProtocolParamsAllegra
    ProtocolParamsMary
    (ProtocolParamsTransition ByronBlock (ShelleyBlock StandardShelley))
    (ProtocolParamsTransition (ShelleyBlock StandardShelley) (ShelleyBlock StandardAllegra))
    (ProtocolParamsTransition (ShelleyBlock StandardAllegra) (ShelleyBlock StandardMary))
  protocolInfo (RunProtocolCardano
               paramsByron
               paramsShelleyBased
               paramsShelley
               paramsAllegra
               paramsMary
               paramsByronShelley
               paramsShelleyAllegra
               paramsAllegraMary) =
    protocolInfoCardano
      paramsByron
      paramsShelleyBased
      paramsShelley
      paramsAllegra
      paramsMary
      paramsByronShelley
      paramsShelleyAllegra
      paramsAllegraMary

-- | Node client support for each consensus protocol.
--
-- This is like 'Protocol' but for clients of the node, so with less onerous
-- requirements than to run a node.
--
class (p ~ BlockProtocol blk, RunNode blk) => ProtocolClient blk p where
  data RunProtocolClient blk p
  protocolClientInfo :: RunProtocolClient blk p -> ProtocolClientInfo blk

instance ProtocolClient ByronBlockHFC ProtocolByron where
  data RunProtocolClient ByronBlockHFC ProtocolByron =
    RunProtocolClientByron EpochSlots
  protocolClientInfo (RunProtocolClientByron epochSlots) =
    inject $ protocolClientInfoByron epochSlots

instance ProtocolClient (ShelleyBlockHFC StandardShelley) ProtocolShelley where
  data RunProtocolClient (ShelleyBlockHFC StandardShelley) ProtocolShelley =
    RunProtocolClientShelley
  protocolClientInfo RunProtocolClientShelley =
    inject $ protocolClientInfoShelley

instance ProtocolClient (CardanoBlock StandardCrypto) ProtocolCardano where
  data RunProtocolClient (CardanoBlock StandardCrypto) ProtocolCardano =
    RunProtocolClientCardano EpochSlots
  protocolClientInfo (RunProtocolClientCardano epochSlots) =
    protocolClientInfoCardano epochSlots
