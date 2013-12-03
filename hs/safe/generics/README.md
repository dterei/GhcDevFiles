# Generics

Originally from [here](https://github.com/JohnLato/safe-bugtest).

Demonstrate a possible bug in SafeHaskell abstraction boundaries using
GHC.Generics.

## Result

Safe. But worrying. Since the Pos is deriving Generic, it is
essentially exporting it's constructors. This is similar to it having
a Read and Show instance and accusing this of being a flaw.

Also since you can only `derive Generic` when you have access to all
constructors this isn't an attack avenue.

However, it may make sense to restrict GHC.Generics anyway (similar to
Typeable) as the benefits outway the cost. How often is some of the
more unsafe functions in that module needed? If rarely then the
benefit from knowing Generics are compiler derived in Safe code is
large and outways the costs.


## Alternatives?

* Make GHC.Generics unsafe and have a GHC.Generics.Safe that exposes
  the needed tools for Generic deriving.

* Add a value or type to deriving that encodes if it was by hand or
  compiler?

* Hand rolled instances of Generic aren't allowed in Safe modules
  (i.e., same rule as Typeable).

