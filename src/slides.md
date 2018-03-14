---
title: Domain Modeling with Haskell Data Structures
author: Oskar Wickström
date: March 2018
theme: Boadilla
classoption: dvipsnames
---

## Domain Modeling

* Clear and unambious naming
* Reify the domain we are working with in our code
* Separate bounded contexts

## Haskell

* All the flexibility we need
    - Sum types
    - Product types
    - Type classes
* Powerful type system and compiler
* Mature language and ecosystem

## Modeling in Haskell

* To a large extent, our domain model should be reified with data types
* Separation with data types:
    - Separate bounded contexts using different data types
    - Define explicit interfaces and translations between them
* Structure computation as data structures
* Use modules for cohesive units
    - Domain logic, repositories/data access, rendering
    - Capture interfaces and responsibilities using type classes
    - Avoid the `Types.hs` trap
* Leverage all the good abstractions in Haskell

## Type-Driven Development

* "Type, define, refine"
* When modifying existing code:
    - Change to data types to model the new behavior
    - Fix all the type errors
    - Test, and possibly refine your model
* Great for changing business requirements
* Focus testing on behaviour

# Example: Project Management System

## Project Management

* Not terribly exciting, but relatable
* We'll explore:
    - Data types
    - Some very useful abstractions

## Projects

![](../uml/project.png){width=50%}

## Project

``` {.haskell include=src/listings/data-structures/src/Project.hs snippet=project}
```

## Budget

``` {.haskell include=src/listings/data-structures/src/Project.hs snippet=budget}
```

## Transaction

``` {.haskell include=src/listings/data-structures/src/Project.hs snippet=transaction}
```

## Reporting

``` {.haskell include=src/listings/data-structures/src/Reporting.hs snippet=report}
```

## Calculating a Report

``` {.haskell include=src/listings/data-structures/src/Reporting.hs snippet=calculateReport}
```

## Aggregating Reports

``` {.haskell include=src/listings/data-structures/src/Reporting.hs snippet=calculateProjectReport}
```

## Printing Projects

``` {.haskell include=src/listings/data-structures/src/PrettyPrint.hs snippet=tree}
```

## Printing Projects in the REPL

\verbatimfont{\small}
```
*Demo> putStrLn (prettyProject someProject)
Sweden
|
+- Stockholm (1)
|
+- Gothenburg (2)
|
`- Malmö
   |
   +- Malmö City (3)
   |
   `- Limhamn (4)
```

## Printing Reports

``` {.haskell include=src/listings/data-structures/src/PrettyPrint.hs snippet=prettyReport}
```

## Printing Reports in the REPL

\verbatimfont{\small}
```
*Demo> p <- calculateProjectReport someProject
*Demo> putStrLn (prettyReport p)
Budget: -14904.17, Net: 458.03, difference: +15362.20
```

## What we've used so far

* Basic Haskell data types
* Explicit recursion
* Monoid
* Functor
* Foldable

# New Requirements!

## A Tree Of Reports

* One big report for the entire project is not enough
* The customer needs them for all individual projects

## Parameterizing Project

``` {.haskell include=src/listings/foldable-traversable/src/Project.hs snippet=project}
```

## Calculating Project Reports with Traversable

``` {.haskell include=src/listings/foldable-traversable/src/Reporting.hs snippet=calculateProjectReports}
```

## Accumulating Reports with Foldable

``` {.haskell include=src/listings/foldable-traversable/src/Reporting.hs snippet=accumulateProjectReport}
```

## Adapting the Pretty Printing

``` {.haskell include=src/listings/foldable-traversable/src/PrettyPrint.hs snippet=pretty-printing}
```

## Pretty Printing the Reports

\verbatimfont{\footnotesize}
```
*Demo> pr <- calculateProjectReports someProject
*Demo> putStrLn (prettyProject prettyReport pr)
Sweden
|
+- Stockholm: Budget: -2259.99, Net: 391.23, difference: +2651.22
|
+- Gothenburg: Budget: -3204.79, Net: -228.31, difference: +2976.48
|
`- Malmö
   |
   +- Malmö City: Budget: -6958.82, Net: 2811.88, difference: +9770.70
   |
   `- Limhamn: Budget: 5856.93, Net: 1941.43, difference: -3915.50
```

## Pretty Printing the Reports (cont.)

\verbatimfont{\footnotesize}
```
*Demo> putStrLn (prettyReport (accumulateProjectReport pr))
Budget: -6566.67, Net: 4916.23, difference: +11482.90
```

## What we've added to our toolbox

* Parameterized Data Type
* Traversable

# "No, that's not what we want."

## Actual Requirements

* The customer wants reporting on *all* levels:
    - project groups
    - single projects
* We need to change our model again

## Parameterizing Project Even More

``` {.haskell include=src/listings/writert/src/Project.hs snippet=project}
```

## Calculating Project Reports with Traversable

``` {.haskell include=src/listings/writert/src/Reporting.hs snippet=calculateProjectReports}
```

``` {.haskell}
    -- ...
```

## Calculating Project Reports with Traversable (cont.)

``` {.haskell include=src/listings/writert/src/Reporting.hs snippet=calculateProjectReports-single}
```

## Calculating Project Reports with Traversable (cont.)

``` {.haskell include=src/listings/writert/src/Reporting.hs snippet=calculateProjectReports-group}
```

## Adapting the Pretty Printing

``` {.haskell include=src/listings/writert/src/PrettyPrint.hs snippet=asTree}
```

``` {.haskell include=src/listings/writert/src/PrettyPrint.hs snippet=prettyProject}
```

## Pretty Printing the Reports

\verbatimfont{\footnotesize}
```
*Demo> pr <- calculateProjectReports someProject
*Demo> putStrLn (prettyProject prettyReport prettyReport pr)
Sweden: Budget: -9278.10, Net: +4651.81, difference: +13929.91
|
+- Stockholm: Budget: -3313.83, Net: -805.37, difference: +2508.46
|
+- Gothenburg: Budget: -422.48, Net: +1479.00, difference: +1901.48
|
`- Malmö: Budget: -5541.79, Net: +3978.18, difference: +9519.97
   |
   +- Malmö City: Budget: -4069.45, Net: +2185.02, difference: +6254.47
   |
   `- Limhamn: Budget: -1472.34, Net: +1793.16, difference: +3265.50
```

## Even More Learnings

* Explicit recursion might still be necessary
* The Writer monad transformer
* There are many ways to leverage Monoid
* Computation as a data structure

# Is there more?

## Explicit Recursion

* Explicit recursion can, with large data types, be error-prone
* Current `Project` type has a hidden coupling to the reporting module
    - The `g` and `a` parameters are only there for reporting
* Can we decouple `Project` from that concern?

## Factoring Out Recursion

``` {.haskell include=src/listings/fixplate/src/Project.hs snippet=Project}
```

## Fix

\verbatimfont{\small}
```
*Project> import Data.Generics.Fixplate.Base
*Project Data.Generics.Fixplate.Base> :t Fix
Fix :: f (Mu f) -> Mu f
```

## Project Constructors

``` {.haskell include=src/listings/fixplate/src/Project.hs snippet=constructors}
```

## Decorating with Attr

``` {.haskell include=src/listings/fixplate/src/Reporting.hs snippet=ProjectReport}
```

## Bottom-Up Reporting with Fixplate

``` {.haskell include=src/listings/fixplate/src/Reporting.hs snippet=calculateProjectReports}
```

## Pretty Printing Without Explicit Recursion

``` {.haskell include=src/listings/fixplate/src/PrettyPrint.hs snippet=prettyResult}
```

## Fixplate Draws Our Trees!

\verbatimfont{\scriptsize}
```
*Demo> pr <- calculateProjectReports someProject
*Demo> drawTreeWith prettyResult pr
 \-- Sweden: Budget: +2191.60, Net: +1238.19, difference: -953.41
      |-- Stockholm (1): Budget: +5092.27, Net: -1472.80, difference: -656 ...
      |-- Gothenburg (2): Budget: -4325.22, Net: +2252.52, difference: +65 ...
      \-- Malmö: Budget: +1424.55, Net: +458.47, difference: -966.08
           |-- Malmö City (3): Budget: -6456.93, Net: +2400.33, difference ...
           \-- Limhamn (4): Budget: +7881.48, Net: -1941.86, difference: - ...
```

## Summary

* Use Haskell data types
* Leverage great abstractions
    - Functor
    - Monoid
    - Foldable
    - Traversable
    - and many more
* Maybe check out recursion schemes
* Enjoy evolving and refactoring existing code

# Questions?

