# Overlapping Instances

GHC allows typeclass instance functions to overlap (ad hoc
polymorphism / subtyping style). This allows any module
to change the behaviour of any other part of the code that
calls a typeclass function by providing a more specific instance
that the one currently being used at that call site.

An attacker can use this to change the behaviour of the TCB
code. Whie you can protect against it by only calling
typeclass functions with concrete instance types (so they
are already the most specific instance) this is not ideal.

## results

Safe. While the ability of OI to redefine existing code is scary
and may require some more thought from the TCB developers, I'm not
sure it can be used as an attack.

Firstly, all attacks I can think of require you to define an more
specific instance in the Unsafe module. Since `Safe` doesn't allowing
more specific overlaps this doesn't work.

If you aren't defining a more specific overlap then you aren't
redefining existing code.

