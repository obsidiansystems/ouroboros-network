# Eras and Consensuses

The Cardano blockchain is getting more sophisticated and gaining new
features over time. Many different versions or eras of the Cardano
blockchain, intended for different stages of the development of the
blockchain, must coexist in one codebase, in the same revision of the
same git repo. This is to allow smooth transitions of all nodes from
one era to another, so the same binaries can support multiple nodes.

Each era, to be usable, must be a member of one or more consensus
protocols or consensus modes, which are each a linear progression of
eras. For example, the Shelley consensus can transition through the
Shelley, Allegra, and Mary eras. In this case, Shelley is both an era
and a consensus protocol.

All production eras are part of the Cardano consensus mode.
Consensus modes also exist for staying in one particular era,
or for testing the Shelley-based eras together, which have some
code in common.

Additionally, there is the Example conseensus mode, designed for
prototyping new eras, and testing transitioning into them from
existing eras. As the eras being prototyped are not production
eras, they are not yet added to the Cardano consensus protocol.

# Implementation of Eras

The implementations of eras live in the
[cardano-ledger-specs](https://github.com/input-output-hk/cardano-ledger-specs/)
repo, and they take the form of a type and a series of instances (of
type classes and type families) associated with it. The type is simply
a tag to attach the instances to and a value for metaprogramming (the c
is a parameter to allow you to specify what crypto algorithms are used,
e.g. Ed25519 for signatures and Blake2B for hashes, with sensible defaults
set in instances for StandardCrypto, which can be used as the standard
value for c):

```
data ExampleEra c
```

The instances convey all the actual implementation decisions that differ between this era and other eras. Some indicate types, like what the actual transaction type looks like:

```
type instance Core.TxBody (ExampleEra c) = TxBody (ExampleEra c)
type instance Core.TxOut (ExampleEra c) = TxOut (ExampleEra c)
```

(Note that in this situation, the convention is to use the same name for the type family in the Cardano.Ledger.Core package, and the specific type indicated in the era-specific package.)

Some indicate rule sets, which are handled by a type-level correspondence
between strings and instances of Control.State.Transition.Extended.STS,
each of which specifies rules, which are implemented in a
Rule monad, which is a free monad of a Clause type defined in
Control.State.Transition.Extended.

For all of these, it is completely appropriate to use a value imported
from another era (e.g. Shelley or Mary) for particular pieces of the
implementation that do not differ from that other era. Reserve adding
new files to the new era for actual new implementation.

For more information on adding eras, and especially in making sure all
the test infrastructure is set up correctly, see [adding an era](https://github.com/input-output-hk/ouroboros-network/blob/master/ouroboros-consensus/docs/AddingAnEra.md).

# Transitioning Eras

In order to support transitions into an era, additional work must be done:
Among the various instances that must be set, each era has a type
instance indicating what the previous era is, so that each era only
has to implement code to transition from one previous era (as on the
production blockchain, eras progress in a linear fashion). These
transitions then actually work if the era is added to a consensus mode
after the era it is supposed to transition from.

```
type instance PreviousEra (ExampleEra c) = ShelleyEra c
```

The actual translation code for various types is written in instances of TranslateEra. For example, the code to translate Shelley-era transactions to the Example era:

```
instance forall c. Crypto c => TranslateEra (ExampleEra c) Tx where
  type TranslationError (ExampleEra c) Tx = DecoderError
  translateEra _ctx tx =
    case decodeAnnotator "tx" fromCBOR (serialize tx) of
      Right newTx -> pure newTx
      Left decoderError -> throwError decoderError
```

# Consensus Protocol Implementation

In this repo, there are projects for individual consensus protocols,
such as `ouroboros-consensus-shelley` and `ouroboros-consensus-byron`.
The overall Cardano consensus mode and the prototyping example
consensus mode are both implemented in `ouroboros-consensus-cardano`,
which also includes a GADT, known as `Protocol` with all the consensus protocols,
in the Ouroboros.Consensus.Cardano module. Adding a new consensus mode to this
GADT is currently necessary to using it in `cardano-node`.
