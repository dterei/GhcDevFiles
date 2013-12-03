# Generics

Originally from [here](https://github.com/JohnLato/safe-bugtest).

Demonstrate a possible bug in SafeHaskell abstraction boundaries using
GHC.Generics.

## Result

Unsafe.

Can use generics to instantiate values of any design. Basically,
Generics functions as a standard proxy for a data type, so contruct
the value you want through GHC.Generics and then use `GHC.Generics.to`
to instantiate the value.

