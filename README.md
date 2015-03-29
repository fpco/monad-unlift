## monad-morphism

FIXME to be written properly, but for now:

we seem to constantly run into situations where we need to use monad-control, and there's a lot of time and brainpower spent on figuring out how to use the library. there are also corner cases about discarding side effects

however, in many cases, we're just dealing with monad morphisms, for which neither of those problems should apply

so `ReaderT`, `LoggingT`, etc, would all apply
