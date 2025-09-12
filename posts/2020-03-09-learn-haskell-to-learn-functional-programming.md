---
layout: post
title: Learn Haskell to learn Functional Programming
date: 2020-03-09
tags: haskell functional-programming
---
Haskell is a great language. It pioneers functional programming and enforces it. If you want to learn functional programming you should seriously consider learning it through Haskell. Let's go through some of Haskell's features to see what makes it special.

## Immutability
You might be aware that we don't mutate data in functional programming. Once a value is defined it stays its same self throughout its existence. That is, once `let name = "fpunfold"` has been declared, `name` can never be set to anything else. It's what is called a constant in some languages.

Haskell enforces immutability and there's no way to mutate any data after its declaration. This forces you to learn about dealing with data in a functional style which is a crucial step in your quest to learn functional programming.

## Syntax
Haskell offers beautiful syntax (that it borrows from mathematics) for fundamental concepts in functional programming. For example, function composition is written as `f . g` in mathematics as well as in Haskell. As you might tell, `(f . g) (x)` applies function `f` to the result of `g(x)`.

```
λ> let f x = x + 3
λ> let g x = x * 2
λ> (f . g) (1)
5
```

Ability to compose functions using a dot opens door for a "Pointfree" style of coding. Pointfree style of coding lets the programmer skip specifying parameters to functions when composing them. This removes the ugly nested parentheses from the code and improves readability. I think Pointfree code looks beautiful.

```haskell
let fn x = f(g(h(x))) -- Pointful
let fn = f . g . h    -- Pointfree, notice that parameter x is skipped
```

## Function purity
Haskell has a clear separation between pure functions and impure functions. A pure function is a function whose value is deterministic and depends solely on its inputs. Mathematical functions are pure. For example, `add 2 2` always equals `4`, `sqrt 4` always equals `2`, and so on. Contrast this with Python's `random.random()` function which returns a nondeterministic value on each call and is thus not pure (impure). A simple way to tell if a function is impure is to check if the function has a side-effect outside of its scope. For example, Python's `random.random()` function implicitly mutates the global state of Python runtime's random number generator whenever it is called. You can check the current state of the random number generator in Python using the `random.getstate()` function. Call it before and after calling `random.random()` and you'd see that the state has changed.

```python
# Python shell
>>> stbefore = random.getstate()
>>> random.random()
0.5207680388690307
>>> stafter = random.getstate()
>>> stbefore == stafter
False
```

Note that all IO operations such as writing to a file, reading from a database, etc. are impure as they cause side-effects and are nondeterministic.

Pure functions are great because they are deterministic and thus easier to reason, debug, and test. In a typical program, most of the logic is pure and if we separate it from impure bits of the program, our program as a whole becomes easier to follow, debug, and test.

In Functional programming, we try to keep as many of our functions pure as possible. In Haskell all functions are by default pure and to make a function impure you need to mark it so. This separation is done using a computational context called the `IO` monad. Think of `IO` monad as an interface that offers impure functions to make side effects.

```haskell
-- whyhaskell.hs
pureAdd :: Int -> Int -> Int 
pureAdd x y = x + y

impureAdd :: Int -> Int -> IO Int -- Result is marked as involving IO
impureAdd x y = do
    putStrLn ("I am adding " ++ show x ++ " and " ++ show y)
    return (x + y)
```

```
-- ghci (Haskell shell)
λ> :l whyhaskell.hs 
[1 of 1] Compiling Main             ( whyhaskell.hs, interpreted )
Ok, one module loaded.
λ> pureAdd 2 2
4
λ> impureAdd 2 2
I am adding 2 and 2
4
```

Thus, coding in Haskell teaches you to properly separate pure and impure bits of your program from each other. This is useful not only to learn functional programming but to write easily tested code when using other paradigms too.

## Currying

Another core concept of functional programming is Currying. Let's see what Currying means using an example. The familiar way of defining a function with multiple arguments is by taking the arguments at once in a single tuple.

```python
# whyhaskell.py
def sayHello(greeting, name):
    return greeting + " " + name + "!"
```

With Currying, instead of making the function take multiple arguments at once in a single tuple, we take the arguments one-by-one, in order. As the function accepts an argument it returns a new function that takes one less parameter. This has tremendous implications on code reuse as shown in the Haskell example below. Haskell evaluates all functions using Currying.

```haskell
-- whyhaskell.hs
sayGreeting :: String -> String -> String
sayGreeting greeting name = greeting ++ " " ++ name ++ "!"

-- Now we define three new functions with different greetings
sayHello = sayGreeting "Hello"
sayHi = sayGreeting "Hi"
sayYo = sayGreeting "Yo"
```

```
-- ghci (Haskell shell)
λ> :l whyhaskell.hs 
[1 of 1] Compiling Main             ( whyhaskell.hs, interpreted )
Ok, one module loaded.
λ> sayHello "fpunfold"
"Hello fpunfold!"
λ> sayHi "fpunfold"
"Hi fpunfold!"
λ> sayYo "fpunfold"
"Yo fpunfold!"
```

## Algebraic Data Types
Functional programming makes heavy use of Algebraic Data Types. These are types that are composed of one or more other types and can be decomposed to perform pattern matching. User defined data types in Haskell are Algebraic thus teaching you another core concept of Functional programming well. See post on <a href="https://fpunfold.com/2020/05/10/algebraic-data-type/">Algebraic Data Type</a> to learn more about them.

## Typeclasses and Types
Functional programming makes use of many concepts such as Functors, Applicatives, Monads, etc. Each of these concepts have some useful properties that simplify programs. Haskell comes with carefully designed built-in typeclasses that correspond to these concepts. Typeclasses declare an interface and types adhering to any typeclasses can be interacted with the interface declared by those typeclasses. You can think of typeclasses as Java interfaces.

To learn functional programming you'd need to learn about some common data types used by functional programmers. Haskell comes with a myriad of such data types. Types such as List, Map, Maybe, Either, State, Reader, Writer, and Zipper adhere to the functional way of thinking and force you to think in a functional way.

## Summary
Learning Haskell is one of the most effective ways to learn functional programming. The only way to code in Haskell is the functional way so it forces you to think in functional style. The language also represents the state of the art in programming language theory and the discipline it brings with it will make you not just a good functional programmer but a good programmer in general.
