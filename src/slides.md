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

# Questions?

