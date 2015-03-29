## monad-morphism

A common pattern is to have some kind of a monad transformer, and want to pass
an action into a function that requires actions in a base monad. That sounds a
bit abstract, so let's give a concrete example:

```haskell
-- From async
concurrently :: IO a -> IO b -> IO (a, b)

func1 :: ReaderT Foo IO String
func2 :: ReaderT Foo IO Double

doBoth :: ReaderT Foo IO (String, Double)
doBoth = _
```

Doing this manually is possible, but a bit tedious:

```haskell
doBoth :: ReaderT Foo IO (String, Double)
doBoth = ReaderT $ \foo -> concurrently
    (runReaderT func1 foo)
    (runReaderT func2 foo)
```

This also doesn't generalize at all; you'll be stuck writing `concurrently`
variants for every monad transformer stack. Fortunately, the `monad-control`
package generalizes this to a large number of transformer stacks. Let's
implement our generalized `concurrently`:

```haskell
concurrentlyG :: MonadBaseControl IO m
              => m a -> m b -> m (StM m a, StM m b)
concurrentlyG f g = liftBaseWith $ \run ->
    concurrently (run f) (run g)
```

Notice how, in the signature for `concurrentlyG`, we no longer return `(a, b)`,
but `(StM m a, StM m b)`. This is because there may be additional monadic
context for each thread of execution, and we have no way of merging these
together in general. Some examples of context are:

* With `WriterT`, it's the values that you called `tell` on
* With `EitherT`, the returned value may not exist at all

In addition to this difficulty, many people find the types in `monad-control`
difficult to navigate, due to their extreme generality (which is in fact the
power of that package!).

There is a subset of these transformer stacks that are in fact [monad
morphisms](http://www.stackage.org/package/mmorph). Simply stated, these are
transformer stacks that are isomorphic to `ReaderT`. For these monads, there is
not context in the returned value. Therefore, there's no need to combine
returned states or deal with possibly missing values.

This concept is represented by the monad-morphism package, which provides a pair of typeclasses for these kinds of transformer stacks. Before we dive in, let's see how we solve our `concurrentlyG` problem with it:

```haskell
concurrentlyG :: MonadBaseMorphism IO m
              => m a -> m b -> m (a, b)
concurrentlyG f g = do
    UnliftBase run <- askUnliftBase
    liftBase $ concurrently (run f) (run g)
```

Notice how we get `(a, b)` in the return type as desired. There's no need to
unwrap values are deal with context.

### MonadTransMorphism

`MonadTransMorphism` is a class for any monad transformer which is isomorphic
to `ReaderT`, in the sense that the environment can be captured and applied
later. Some interesting cases in this space are:

* `IdentityT` and things isomorphic to it; in this case, you can think of the environment as being `()`
* Transformers which contain a mutable reference in their environment. This allows them to behave like stateful transformers (e.g., `StateT` or `WriterT`), but still behave the monad morphism laws. (See below for more details.)

Due to weaknesses in GHC's ImpredicativeTypes, we have a helper datatype to
allow for getting polymorphic unlift functions, appropriately named `Unlift`.
For many common cases, you can get away with using `askRun` instead, e.g.:

```haskell
bar :: ReaderT Foo IO ()

baz :: ReaderT Foo IO ()
baz = do
    run <- askRun
    liftIO $ void $ forkIO $ run bar
```

Using `Unlift`, this would instead be:

```haskell
    Unlift run <- askUnlift
    liftIO $ void $ forkIO $ run bar
```

or equivalently:

```haskell
    u <- askUnlift
    liftIO $ void $ forkIO $ unlift u bar
```

### MonadBaseMorphism

`MonadBaseMorphism` extends this concept to entire transformer stacks. This is
typically the typeclass that people end up using. You can think of these two
typeclasses in exactly the same way as `MonadTrans` and `MonadIO`, or more
precisely `MonadTrans` and `MonadBase`.

For the same ImpredicativeTypes reason, there's a helper type `UnliftBase`.
Everything we just discussed should transfer directly to `MonadBaseMorphism`,
so learning something new isn't necessary. For example, you can rewrite the
last snippet as:

```haskell
    u <- askUnliftBase
    liftIO $ void $ forkIO $ unliftBase u bar
```

### Reference transformers

When playing transformer stack games with a transformer like `StateT`, it's
common to accidentally discard state modifications. Additionally, in the case
of runtime exceptions, it's usually impossible to retain the state. (Similar
statements apply to `WriterT` and `RWST`, both in strict and lazy variants.)

Another approach is to use a `ReaderT` and hold onto a mutable reference. This
is problematic since there's no built in support for operations like `get`,
`put`, or `tell`. What we want is to have a `MonadState` and/or `MonadWriter`
instance.

To address this case, this package includes variants of those transformers that
use mutable references. These reference are generic using the
[mutable-containers](http://www.stackage.org/package/mutable-containers)
package, which allows you to have highly efficient references like `PRef`
instead of always using boxed references like `IORef`.

### conduit

The `transPipe` function in conduit has caused confusion in the past due to its
requirement of provided functions to obey monad morphism laws. This package
makes a good companion to conduit to simplify that function's usage.

### Other notable instances

Both the `HandlerT` transformer from yesod-core and `LoggingT`/`NoLoggingT` are
valid monad morphisms. `HandlerT` is in fact my first example of using the
"enviornment holding a mutable reference" technique to overcome exceptions
destroying state.
