-- # ADT's and Functions
-- I've logically a ADT that consists of multiple constructors. However, I want
-- to write functions that only make sense for 1 of the constructors at most.
-- How best to do this?

data Data = A | B | C

-- Lift each constructor into its own data type.
-- Easy. But adds extra layer of indirection to types (does UNPACK solve?).
-- Also somewhat ugly with extra indirection for coding (e.g., lots of
-- unwrapping, namespace control...)
data Data = MkA A | MkB B | MkC c
data A = A
data B = B
data C = C

-- GADTs?
-- Keep single data type but use type variations of GADT's to choose between
-- constructors in the type system.

-- Typeclass?
-- Split each constructor into its own data type, but unify them through a type
-- class. This is open Vs. closed, which may be good or bad (although can we
-- restrict type class exportation such that it is closed?). Can we also
-- construct some of the typeclass functions using Generic Deriving? or TH?


-- # Let Vs Bind
-- Found the difference in syntax ugly and confusing. He (claimed and 'seemed
-- to) understood the difference and why it existed but he still rejected it.
-- e.g., let x = ....
-- y <-
--
-- In response to this he was using 'sequence' somewhat arbitarily as he often
-- passed around lists of '[IO a]'.


-- # IO, [], and <$>
-- He used <$> a lot! basically he was thinking of it more as a shorter, infix
-- notation of map. He often applied it for IO actions, ending up with [IO a]
-- and then using sequence (often later, somewhere else).
-- a :: [IO a]
-- b :: IO a


-- # Constructor fields
-- He wasn't using record syntax at all and was asking if there were any rules
-- for how many fields a constructor should have. (e.g., mentioned a rule that
-- functions should have 3 arguments at most). He wasn't aware of how to use
-- record syntax that much, also asked if he could use it in pattern matching
-- (had to show him how to use it through guards).


-- # Namespacing
-- Didn't have a good idea of how to handle this.


-- # TH & Generic Deriving
-- Wanted to do something like have data types that all had some common set of
-- fields for using in generic (reflection) based code. Wasn't sure how to
-- handle this due to namespace issues.
--
-- e.g.,
-- module Prezi.A where
-- 
-- data A = A {
--         field1 :: String,
--         field2 :: String
--     }
-- 
-- $[derviveAB A]
-- 
-- module Prezi.B where
-- 
-- data B = {
--         field1 :: String,
--         field2 :: String
--     }
-- 
-- deriving instance AB B
-- 
-- class AB a where
--     toFilePath :: a -> String
-- 
-- instance AB' A
-- instance AB' B


-- # mapM_, sequence, void
-- Wasn't aware of mapM_, thought it was a horrible name.
-- In general he was writing his own functions too much (e.g., had own version
-- of and, all, any...) and so ended up with lots of let's in his code and
-- nesting of case. Found this ugly but wasn't sure how to handle.
-- e.g., *I think a mindset shift in haskell is thinking in units of functions,
-- not of writing own statements*.


-- # Shortcutting IO
-- His code had some deep case nesting in IO functions. In an imperative
-- language this wouldn't have been the case statements where on a bool and the
-- False case meant exit the IO function returning some error condition. How to
-- best represent this in Haskell?


-- # Common fields of ADT's
-- He had one case where he had a bunch of data types that took an array of
-- dates, e.g., Data Rule = RuleTable ..... [Dates] | RuleFile .... [Dates]
-- (there may have been some nesting of data types involved, so [Dates]
-- potentnially wasn't on the same level for all). He had functions that took a
-- date for constructing the various rules. (some where simply per-day rules,
-- some were 'for the last 30 day' rules... e.t.c.,). I wonder if rather than
-- use straight functions to pipe in the day if he could have used a top level
-- data type to wrap Rules and represent the [Days], or even some typeclass
-- (e..g, class RuleDates where { rule :: Rule, dates :: [Days] }


-- # Argument passing
-- Wasn't sure how to handle... (e.g., had some code that handled it fine, but
-- thought it wasn't very 'haskell like'). This was a common thought, he knew
-- how he'd write it well in Python or C++ and mostly wrote good Haskell code
-- to handle it but wasn't confident in it at all as he though that the
-- 'Haskell way' should be quite different to the Python way... temptation to
-- over-engineer or think what the 'Haskell way' was.


-- # TH
-- Mention he looked at it but found it incredibly ugly. E.g., mentioned that
-- in C++ he has used Templates and while they are ugly, found TH even worse.


-- # TArray
-- Mentioned that 3 good engineers spent a few hours trying to use TArray and
-- couldn't figure it out from the documentation, it was too poorly written and
-- sparse. They found this incredibly discouraging.

