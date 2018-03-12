---
title: Domain Modeling with Haskell Data Structures
author: Oskar Wickström
date: March 2018
theme: Boadilla
classoption: dvipsnames
---

# Domain Modeling with Haskell Data Structures

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
* Use modules for cohesive units
    - What could be classes in OOP
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

## Project Managament Data Types

``` {.haskell include=src/listings/data-structures/src/Project.hs snippet=project}
```

# Questions?

