# Consensus style guide

This document describes the style we follow in the consensus team. While style
is subjective, we attempt to motivate our choices with *objective reasons* where
possible. Not everyone agrees with every choice we make, but we compromise. It
is unlikely that this style guide matches the preference of even a single team
member for 100%. In different teams, with different values and priorities,
different choices might make more sense.

This style guide is in particular targeted at newcomers to the team, to get
acquainted with our style. We expect newcomers to try to follow our style to the
best of their ability, just like one tries to respect the house rules when
visiting someone's house or the local laws when visiting another country. While
it should in general be possible up to a certain extent to adhere to the team's
style by imitating the surrounding code's style, it is helpful to have this
style guide as a reference in case of uncertainty, especially when the code in
question is written in an outdated style.

## Guiding principles

We value the following principles in the consensus team:

* __Optimise for clarity__: the following things occur with decreasing
  frequency:

  1. One *reads* the code and tries to understand it.
  2. After having understood the code, one *modifies* it.
  3. One *writes* the initial version of the code. This only happens once.

  We believe it is important to optimise for (1), then (2), but *not* for (3).
  Making a little effort to make to code easier to understand when writing or
  modifying it pays off dividends when reading it the next time, especially when
  another team member will be the one reading it. Picking good names, formatting
  it in a clear way, separating concerns, highlighting the differences between
  similar steps, documenting why it does this and that, ..., are all worth the
  extra effort.

  The layout and formatting of the code can help convey the meaning and the
  essence of the code. Alignment can help emphasise the similarities *and* the
  differences between cases.

  This is why we do not believe in automatic code formatters: they have no idea
  of the story the code is trying to tell. While the convenience, automation,
  and uniformity are big advantages, in our opinion, they come at the cost of
  code clarity.

  We do not optimise for "diff-ability", as one can always hide whitespace
  changes.

* __Consistency__: if everybody does their own thing, we end up with a mix of
  differing styles. This gives a sloppy impression and does not inspire
  confidence in the quality of the code. Compare it to a written text or a CV:
  inconsistent formatting and typos will bother the reader and distract them
  from the real content. If the author didn't even bother to get the formatting
  right or didn't even bother to proofread their own text, why should the reader
  bother with reading the text at all?

  For this reason, we value consistency and tidiness.

## Formatting

We now list the formatting rules we have converged on. As these have grown
organically, not all code follows these rules. When touching some existing code,
we in general recommend sticking to the existing style, but when it differs from
the rules below, it is good practice to update the code's style to match them.

1. __Indentation__: we indent by 2 spaces.

   *Why:* to avoid wasting horizontal screen space.

   There are a few exceptions:

   a. From the start of a function or variable body, we indent by 4 spaces to
      leave room for a `where`-clause, which is indented by 2 spaces.
      ```haskell
      foo x y z =
          ..
        where
          a = ..
      ```

      Note that the definitions in the `where`-clause are indented by 2 spaces
      from `where`. The `where` keyword acts as a separator between the body and
      the bindings. Keeping them at the same indentation level would make it
      hard to see where the body ends.

   b. We indent record `data` and `newtype` definitions as follows:
      ```haskell
      data Foo = Foo {
            fooBar      :: Int
          , fooArgument :: Bool
          }
        deriving (Show, Eq)

      newtype Foo = Foo {
            unFoo :: Int
          }
        deriving (Show, Eq)
      ```
      Note the location of the opening brace, the leading commas, the
      indentation of the second line (6 spaces), the line with the closing brace
      (4 spaces), the line containing `deriving` (2 spaces).

      *Why:* so multiple deriving clauses using `DerivingStrategies` can be
      aligned:

      ```haskell
      data Foo = Foo {
            fooBar      :: Int
          , fooArgument :: Bool
          }
        deriving stock    (Show, Eq, Generic)
        deriving anyclass (NoThunks, NFData)

      newtype Foo = Foo {
            unFoo :: Int
          }
        deriving stock   (Show)
        deriving newtype (Eq)
        deriving NoThunks via InspectHeapNamed "Foo" Foo
      ```
      TODO parentheses around `NoThunks`?

   c. We indent `data` definitions with multiple constructors as follows:
      ```haskell
      data Foo =
          Bar Int Int
        | Baz
            Int
            Int
            (Maybe Bool)
            [Foo]
      ```
      Note the argument of `Baz` being indented by two spaces.

   d. TODO relative indentation: Edsko prefers the former, Thomas the latter.
      ```haskell
      let fooBarBaz = fooBar
                        baz
      -- vs
      let fooBarBaz =
            fooBar baz
      ```

   e. Prefer the "hanging" `do` over starting a line with `do`:
      ```haskell
      -- NO
      foo .. =
        do bar
           baz

      -- YES
      foo .. = do
          bar
          baz

      -- NO, unless first line too long
      foo .. =
          atomically $ do
            bar
            baz

      -- YES
      foo .. = atomically $ do
         bar
          baz
      ```

   f. There are more exceptions when wrapping lines, see __Line length__.

2. __Line length__: we limit the number of characters per line to 80.

   *Why:* while we are not programming with punch cards or typewriters anymore,
   we still think 80 characters is a good limit. Not everybody is writing their
   code in a maximised window on their (ultra)wide monitor. One often has
   multiple windows open next to each other. Too long lines require horizontal
   scrolling, which is annoying. There is also a reason why books and newspapers
   limit the line length, because too long lines make it harder to read, as it
   is easy to lose track on which line you were when going to the next.

   If you are going beyond 80 characters, wrap the line, introduce local
   bindings, etc.

   Comments and docstrings should also be wrapped at 80 characters.

   There are a few exceptions:

   * Sometimes alignment trumps line length. When many lines are aligned and a
     few of them are too long because of that, the benefit of aligning them can
     outweigh the line length limit.
   * Diagrams, examples, or long URLs in the comments can be wider than 80
     characters.

   For certain constructs we have concrete recommendations on how to wrap them
   in case their length exceeds 80 characters:

   a. Type signatures: if a type signature doesn't fit on one line, wrap it like
      so:
      ```haskell
      fooBar ::
           a
        -> ..
        -> ..
      ```

      Note that the first argument is indented by 5 spaces.

      *Why:* so that the first argument is aligned with the other arguments.

      When there are constraints:
      ```haskell
      fooBar ::
           (Eq a, ..)
        => a
        -> ..
        -> ..
      ```

      Note that the `::` stays on the same line as the identifier.

      *Why:* easy grep-ability for the definition.

      When there is an explicit `forall`:
      ```haskell
      fooBar ::
           forall a .. z. (Eq a, ..)
        => a
        -> ..
        -> ..
      ```

      If the `forall` line gets too long, wrap it after the `.`:
      ```haskell
      fooBar ::
           forall a .. z.
           (Eq a, ..)
        => a
        -> ..
        -> ..
      ```

      Note that the `.` after the `forall` stays on the same line and that there
      is no space before it.

      If the constraints don't fit on one line:
      ```haskell
      fooBar ::
           forall a .. z.
           ( Eq a
           , ..
           )
        => a
        -> ..
        -> ..
      ```
      TODO @edsko `(` on previous line?

      If there is a large function argument in the type signature:
      ```haskell
      fooBar ::
        => a
        -> (   forall c. Eq c
            => c
            -> ..
            -> ..
           )
        -> ..
      ```
      Note that the first arrow in the function argument is indented one space
      relative to the opening parenthesis. The above line wrapping rules apply
      to the nested function type as well.

   b. Function calls: when not all arguments to a function call fit on a single
      line, either introduce clear local bindings for the arguments or put each
      argument on a separate line, indented 2 spaces from the function call:
      ```haskell
      -- NO
      fooBar x (baz + 1) bar
        (foo (bar x))

      -- NO
      fooBar x
             (baz + 1)
             bar
             (foo (bar x))

      -- NO
      fooBar
        x (baz + 1)
        bar (foo (bar x))

      -- YES
      fooBar
        x
        (baz + 1)
        bar
        (foo (bar x))
      ```
      *Why:* this makes it easier to see how many arguments there are.

   c. Class or instance contexts: when a class or instance declaration doesn't
      fit onto a single line because of the super-class context, wrap the line
      before the `=>` and align the class name with the first character in the
      context:
      ```haskell
      class (Eq a, ..)
         => C a where

      instance (Eq a, ..)
            => C a where
      ```

      When the context doesn't fit onto a single line, wrap as follows:
      ```haskell
      class ( Eq a
            , ..
            ) => C a where

      instance ( Eq a
               , ..
               ) => C a where
      ```

   d. Tuples in type signatures:
      ```haskell
      foo ::
           a
        -> ( ..
           , ..
           )
        -> ( ..
           , ..
           )
      ```

   e. Data types:
      ```haskell
      data Foo =
          Bar
           Arg1
           Arg2
           ..
           ArgN
        | Baz

      data Foo = Foo {
          , longFieldName ::
                 HasCallStack
              => Int
              -> ..
          }
      ```

   f. Type synonyms:
      ```haskell
      type Foo a b =
        AVeryLongTypeHereAndItKeepsGoing
          Arg1
          (Maybe b)
          Arg3

      type Cts a =
        ( Eq a
        , ..
        , ..
        )
      ```

   g. Function composition:
      ```haskell
      foo =
            h
          . g
          . f
      ```
      Note that the first line is indented by 6 spaces instead of 4.

      *Why:* so that all composed functions and dots are aligned.

3. __Parentheses__: avoid redundant parentheses, except when they help with the
   order of operations.

   *Why:* TODO avoid clutter?

   ```haskell
   -- NO
   foo (Bar x) = (Bar (succ x))
   -- YES
   foo (Bar x) = Bar (succ x)

   -- NO
   ((x + y), z)
   -- YES
   (x + y, z)


   -- OKAY
   (fromIntegral x) * y
   ```

   TODO:
   ```haskell
   -- NO
   instance (Ord a) => .. where
   -- YES
   instance Ord a => .. where

   -- NO
   foo :: (Ord a) => ..
   -- YES
   foo :: Ord a => ..

   -- BUT
   deriving (Eq)
   ```

4. __Spaces__: surround binary operators with a space on each side. A comma is
   *always* followed by a space.

   *Why:* this is a general convention that is also used in text and math books.
   Not doing so makes it harder to read and is sloppy.

   ```haskell
   avg x y = (x + y) / 2

   let ((x, y), z) = foo
   in (y, z)
   ```

   The only exception is when the comma is followed by a closing parenthesis:
   ```haskell
   (,) <$> foo <*> bar

   (True,) <$> foo
   ```
   TODO: what about `(True,,False)`?

6. __Function composition and the dollar operator__: TODO ` .. . .. . .. $ ..`

    TODO Preference over parentheses?
    TODO `return . Config $ foo $ x` vs `return $ Config $ foo x`

    When a function or constructor takes two or more arguments that are the same
    "level", one should prefer parentheses over the `$`-operator, as the latter
    gives the false impression that it is a function pipeline:

    ```haskell
    -- NO
    init $ Config bar $ succ baz
    -- YES
    init $ Config bar (succ baz)
    ```

7. __Opening braces__: we don't start a new line for opening braces:

   ```haskell
   data Foo = Foo {
         ..
       , ..
       }

   mkFoo x = Foo {
         ..
       , ..
       }

   modifyFoo foo = foo {
         ..
       , ..
       }

   bar foo = ..
     where
       Foo {
           ..
         , ..
         } = foo
   ```
   *Why:* for consistency with `instance .. where` and `class .. where`, which
   we also don't put on a new line.

8. __Blank lines__: we use *exactly one blank line* between different
   declarations: export lists, import lists, declarations, etc.

   *Why:* a blank line helps with readability. Always using a single one is
   consistent and easier to adhere to than one line in these cases and two lines
   in those other cases.

   When defining multiple non-trivial bindings in a `where`-block, separate them
   with a single blank line.

   ```haskell
   fooBar .. =
       ..
     where
       foo :: ..
       foo = ..

       bar :: ..
       bar = ..

   -- OKAY
   foo .. =
     where
       x = ..
       y = succ x
   ```

   Always end a file with a *newline*, which is not the same as a blank line.
   ```
   -- NO
   ..

   <EOF>

   -- NO
   ..<EOF>

   -- YES
   ..
   <EOF>
   ```
   *Why:* see [this StackOverflow answer](posix-line), moreover, GitHub will
   highlight a missing newline at the end of the file.

   [https://stackoverflow.com/questions/729692/why-should-text-files-end-with-a-newline]: posix-line

9. __Sections__: we group related definitions in sections that start with a
   section title. The same grouping can be replicated in the export list.

    ```haskell
    {-------------------------------------------------------------------------------
      Foo
    -------------------------------------------------------------------------------}

    data Foo = ..

    mkFoo :: ..

    ..


    {-------------------------------------------------------------------------------
      Bar

      Bar is bla bla
    -------------------------------------------------------------------------------}

    type Bar = ..
    ..
    ```

    The two lines of the section header are each 80 characters in total. The
    title is indented by two spaces. The section header can contain more text,
    which is separated from the first line by one blank line. The section header
    has a single blank line above and below it.

10. __Comment style__: in general we prefer `--` over `{- .. -}`. We sometimes
    make exceptions for big non-Haddock comments.

    TODO

11. __Haddock formatting__: we use [Haddock formatting](haddock-formatting) in
    docstrings. We also do this in comments for consistency.

    ```haskell
    -- | Short title
    --
    -- Longer description .. 'Foo' .. "Data.String" .. @a@ .. /not/ ..
    -- __never__ .. called \"foo bars\" .. alternative style " foo bars "
    -- .. @'Foo' a@
    --
    -- > foo bar baz
    --
    -- .. ends here.
    foo :: ..
    ```
    Note the space before and after the `|`. We do not align the following lines
    with the first character of the `|`.

    Haddock treats something between double quotes as a link to a module. So
    when you try to quote something, either use backslashes or extra spaces as
    in the example above.

    We prefer `-- |` over `-- ^`. We only use the latter when documenting the
    arguments to a constructor or a function:
    ```haskell
    foo ::
         Word  -- ^ Max size
      -> ..

    data Foo =
        -- | Foo
        --
        -- ..
        Foo
         Int  -- ^ @x@
         (Maybe Bool)
         -- ^ .. long line ..

        -- | Baar
      | Baar
    ```

    Note the indentation of `-- |`, the two spaces before the `-- ^`, and the
    blank line between the constructors.

    We often document preconditions, invariants, and postcondition using the
    following style:

    ```haskell
    -- | Foo
    --
    -- PRECONDITION: x must be greater than y
    -- > x > y
    --
    -- POSTCONDITION: the result will be positivei
    foo :: ..

    data Foo = Foo {
          -- | The bar ..
          fooBar :: Int

          -- | The baz ..
          --
          -- INVARIANT: 'fooBaz' is always greater than 7
        , fooBaz :: Int
        }
    ```

    [https://www.haskell.org/haddock/doc/html/ch03s08.html]: haddock-formatting

12. __Alignment__: we align things when it helps with readability. TODO
    subjective, when? We don't align fields when separated by docstrings,
    doesn't help.

    Alignment makes it clear which things are the *same* and which things are
    *different*, compare the following code block with the aligned version
    below:
    ```haskell
    foo (Quux a b c) = bar a b c
    foo (Bar b c) = bar [] b c
    foo (FooBar a c) = bar a [] c
    ```
    ```haskell
    foo (Quux   a b c) = bar a  b  c
    foo (Bar      b c) = bar [] b  c
    foo (FooBar a   c) = bar a  [] c
    ```
    Alignment makes it easier to spot errors. For example, compare the two code
    blocks, where the `c` argument is forgotten on the second line:
    ```haskell
    foo (Quux a b c) = bar a b c
    foo (Bar b c) = bar [] b c
    foo (FooBar a c) = bar a []

    ```haskell
    foo (Quux   a b c) = bar a  b  c
    foo (Bar      b c) = bar [] b  c
    foo (FooBar a   c) = bar a  []
    ```
    It is immediately obvious in the aligned code, but not in the unaligned
    code.

    Maintaining this alignment when modifying the code only requires little
    effort and is just polite.

13. __Pattern guard alignment__: TODO

    ```haskell
    foo x y z
        | x == y
        = ..
        | Just z' <- z
        , z' == x
        , let x' = ..
        = .. x'
        | otherwise
        = ..
      where
        ..
    ```
    TODO @edsko guards (4) vs `where` (2)

    We put the `->` in a `case` expression on separate lines so that we can
    align them.
    ```haskell
    case mX of
      Just x
        | x > 100
        -> ..
        | x > 0
        -> ..
      _otherwise
        -> ..
    ```
    Note the use of `_otherwise` instead of `_`, as the latter is easy to miss.

14. __if-then-else__: prefer pattern guards over `if-then-else`.

    *Why:* `if-then-else` is less extensible and less Haskell-y than pattern
    guards. Pattern guards can be added without an additional level of nesting.

    When using `if-then-else` in combination with `do`, follow the following
    style:
    ```haskell
    if foo then do
      bar
      baz
    else do
      quux
      bar
    ```

    *Why:* to avoid wasting horizontal screen space.

14. __Naming__: TODO

   TODO https://kowainik.github.io/posts/naming-conventions

   TODO record fields?

   TODO `newtype Foo`: `getFoo`, `unFoo`, `runFoo`?

15. __Import lists__: we use `stylish-haskell` to automatically format import
    lists. See the `.stylish-haskell.yaml` file in this repo.

    We prefer the following order of import groups that has to be maintained
    manually:

    ```haskell
    -- Prelude
    import           Prelude hiding (..)

    -- base + third-party non-Cardano packages
    import           Control.Monad (mplus)
    import           Data.Text (Text)
    import qualified Data.Text as Text
    import           NoThunks.Class (NoThunks)

    -- cardano-prelude
    import           Cardano.Prelude (forceElemsToWHNF)

    -- cardano-base
    import           Cardano.Binary (ToCBOR)

    -- ouroboros-network and other network packages, each namespace in a separate group
    import           Ouroboros.Network.Block (Serialised)

    -- ouroboros-consensus
    import           Ouroboros.Consensus.Block

    -- Storage layer
    import           Ouroboros.Consensus.Storage.ChainDB (ChainDB)

    -- cardano-ledger-specs
    import qualified Shelley.Spec.Ledger.API as SL

    -- ouroboros-consensus-shelley (or mock or byron)
    import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)

    -- ouroboros-consensus-cardano
    import           Ouroboros.Consensus.Cardano.Block
    ```
    Each group is of course optional and must *not* be preceded by the comment
    like in the example above.

    The idea behind the ordering is to start with the most general
    packages/modules and then go more and more specific, ending with the local
    package. In general, an import group will only depend on packages in import
    groups *above* it, *not below* it. For example, the network layer import
    group comes *before* the consensus import group, as the latter depends on
    the former. The Shelley ledger import group comes before the Shelley ledger
    consensus integration import group. In case of ties, i.e., when multiple
    import groups don't depend on each other, we have no real preference. We do
    put Byron before Shelley.

    TODO what about test imports?

    When importing modules from consensus and in particular modules from the
    same package, an import list and a qualifier can be omitted. For example,
    importing `Ouroboros.Consensus.Block` is often done without an import list
    as it brings many basic definitions that are relied upon in scope.

    When importing from other packages, we prefer to use either an import list
    or a qualifier.

16. __Export lists__: we format export lists in the following way:

    ```haskell
    module X (
        ..
      , ..
      ) where
    ```

    We sometimes use Haddock headings:
    ```haskell
    module X (
        -- * Foo
        ..
        -- ** Foo Bar
      , ..
        -- * Bar
      , ..
      ) where
    ```

    TODO When exporting something with members, e.g., a data type with
    constructors or a class with methods, we format them in the following way
    (note the space):
    ```haskell
    module X (
        Foo (..)
      , Bar (MkBar)
      ) where
    ```
    *Why:* this is consistent with how `stylish-haskell` formats it when
    importing it.

    When intentionally hiding the constructor of a data type or newtype, we add
    a `-- opaque` comment after it in the export list to be explicit about this:
    ```haskell
    module X (
        Foo -- opaque
      ) where
    ```
    *Why:* otherwise, people unfamiliar with this type might be tempted to
    export its constructor without realising they're hidden for a reason. This
    comment should make them (and the reviewer) think twice.

    When re-exporting several modules from one module, use the following pattern:
    ```haskell
    module Foo (module X) where

    import Foo.A as X
    import Foo.B as X
    import Foo.C as X
    ```
    *Why:* one can add extra imports without having to modify the export list.

17. __Syntactic extensions__: we like to use some syntactic language extensions.
    Some argue against having to learn additional syntax, but we believe the
    learning curve is minimal and using them can help improve the clarity of the
    code.

    We like to use `LambdaCase` to avoid giving intermediate results a redundant
    name:
    ```haskell
    -- OKAY
    mFoo <- getFoo
    case mFoo of
      Nothing  -> ..
      Just foo -> ..

    -- OKAY
    getFoo >>= \case
      Nothing  -> ..
      Just foo -> ..
    ```
    In the second snippet, there was no need to name the intermediary `mFoo`
    result. Especially when its name is long or coming up with a reasonable name
    for it is tricky, we recommend using `LambdaCase`.

    The use of `MultiWayIf` is also recommended when it improves the
    readability:
    ```haskell
    if | Set.member pt prevApplied -> Just True
       | Map.member hash invalid   -> Just False
       | otherwise                 -> Nothing
    ```
    In our opinion, this is more readable than alternatives like:
    ```haskell
    if Set.member pt prevApplied then Just True else
    if Map.member hash invalid   then Just False else
                                      Nothing
    ```

19. __Records__: TODO
    * fields to (de)construct
    * field names, DuplicateRecordFields
    * no partial fields
    * pattern on records in local bindings
    * NamedFieldPuns vs RecordWildCards

20. __Pointfree__: TODO

21. __Warnings__: we use the following warnings for each Cabal component:
    ```haskell
    -Wall
    -Wcompat
    -Wincomplete-uni-patterns
    -Wincomplete-record-updates
    -Wpartial-fields
    -Widentities
    -Wredundant-constraints
    -Wmissing-export-lists
    ```
    *Why:* the warnings produced by the above list of flags signal code smells
    or enforce good practices. There is seldom a reason to disable one of them.
    At the time of speaking, we haven't needed any CPP yet to accomplish this.

    We sometimes make exceptions for test code, e.g.,
    `-Wno-incomplete-uni-patterns`.

    We keep code warning-free and enforce it by using `-Werror` in CI.

    Always use `-Wx` and `-Wno-x` instead of `-fwarn-x` and `-fno-warn-x`.

22. __HasCallStack__: when using `error` in code paths should be impossible and
    are indicate of bugs, make sure enough `HasCallStack` constraints are in
    scope so that the error message will result in a useful callstack.

    Note that `HasCallStack` constraints on record fields will need manual
    wrappers to work properly:
    ```haskell
    data API m = API {
          foo_ :: HasCallStack => Maybe a -> m a
        }
    foo :: HasCallStack => API m -> Maybe a -> m a
    foo = foo_
    ```
    Without the extra wrapper `foo`, the call stack would only start at `_foo`,
    which is rather useless.

23. __Ambiguous types__: we avoid `AllowAmbiguousTypes`. Instead, we add a
    `Proxy` argument for the ambiguous type variable.

    *Why:* this makes it explicit which type variable is ambiguous.

    When passing the `Proxy`, use `Proxy @X` where `X` is the concrete type.

    *Why:* this is less verbose than `Proxy :: Proxy X`.

    In general, use type application instead of a type annotations, as the
    latter is in most cases more verbose.

    When the same `Proxy` can be used multiple times, one can define it locally
    like so:
    ```haskell
    pb :: Proxy blk
    pb = Proxy
    ```

24. __Redundant pragmas__: remove unused language pragmas when possible.

    *Why:* if a module lists the `CPP`, `AllowAmbiguousTypes`,
    `UndecidableInstances`, or any other suspicious extension, it triggers an
    unnecessary red flag. Even for harmless extensions, it is good practice to
    remove unused ones.

    *Tip:* HLint can warn you about unused pragmas.

## Guidelines

There are more general guidelines on how we write and structure code.

1. __Local bindings__: TODO let vs where, multiple lets vs a single let?

2. __`newtype` vs `data`__: TODO

3. __`type` vs `newtype`__: TODO

4. __Super class vs constraint synonym__: TODO

5. __type family vs data family__: TODO

6. __Tuples__: TODO tuples with more than 2 arguments

7. __Abstract types__: TODO

    parametricity, use type variables instead of concrete types so the type check will protect you
    return vs pure
    HasHeader blk vs (StandardHash blk, Typeable blk)

8. __API records__: TODO

    Mempool.API, Mempool.Impl, Mempool
    mocking
    functions derived in terms of the API
    only program against .API

9. __Lazy vs strict__: TODO

10. __Orphans__: TODO

11. __Assertions__: TODO

12. __Test packages__: TODO our -test packages trick
    TODO avoid sharing source folders at all cost


