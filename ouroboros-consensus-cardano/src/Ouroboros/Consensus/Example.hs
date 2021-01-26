{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Ouroboros.Consensus.Example (
    -- * The block type of the Cardano block chain
    ExampleBlock
    -- * Supported protocols
  , ProtocolExample
  ) where

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Example.Block

{-------------------------------------------------------------------------------
  Supported protocols

  We list these as explicit definitions here (rather than derived through
  'BlockProtocol'), and then /verify/ in 'verifyProtocol' that these definitions
  match. This provides an additional sanity check that we are not accidentally
  breaking any assumptions made in @cardano-node@.
-------------------------------------------------------------------------------}

type ProtocolExample = HardForkProtocol '[ ShelleyBlock StandardShelley
                                         , ShelleyBlock StandardExample
                                         ]
