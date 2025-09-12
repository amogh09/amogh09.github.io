---
layout: post
title: Algebraic Data Type
date: 2020-05-10
tags: haskell functional-programming
---
Algebraic Data Type is a combination of one or more data types. Algebraic Data Types are categorized into two categories - Product Types and Sum Types. Algebraic Data Types are decomposed back into their components using pattern matching. They are used extensively in Functional Programming because of their pattern matching feature.

Product Type is a type created by a composition of zero or more data types. It is called a Product Type as the number of possible values of this type is the product of the number of possible values of each of its component types. A tuple is an example of a Product Type. Composition of data types in Product Types is represented using an arbitrary Symbol called Data Constructor. Data constructors are used to perform pattern matching against Algebraic Data Types. In the following Product Type example, we define a `Person` data type that composes a `String` type (holding the person's name) and an `Int` type (holding the Person's age). The data constructor used to compose these two component types together is `Pers`.

```haskell
-- algebraic.hs (Haskell source file)
data Person = Pers String Int
```

```haskell
-- ghci (Haskell shell)
λ> :l algebraic.hs 
[1 of 1] Compiling Main             ( algebraic.hs, interpreted )
Ok, one module loaded.
λ> let p1 = Pers "fpunfold" 1
```

Sum Type is an Algebraic Data Type which is a union (or sum) of several different Data Types. A value of a Sum Type belongs to one of these types. For example, a move in the game rock-paper-scissors is either Rock or Paper or Scissors. We can define it as `data Move = Rock | Paper | Scissors` in Haskell. The number of possible values of a Sum Type is the sum of the number of possible values of each of its classes.

Let's see some common Algebraic Data Types.

## Lists
(Linked) List is the most fundamental data structure in functional programming. List is a Sum Type that is either Empty or a composition of a Data Type (list's head element) and another list of the same type (list's tail). The Data Constructor (composition symbol) used for lists is usually `cons` (written as a prefix to head and tail components) or `:` (written infix between head and tail components). Empty list is usually represented by `[]`. List `[1,2,3]` can thus be represented as `cons 1 (cons 2 (cons 3 []))` or `1 : 2 : 3 : []`.

```haskell
data List a = cons a (List a) | []
```

## Maybe
Maybe (also called Optional or Option) is an Algebraic Data Type used to represent an optional value. It is a sum type that is either nothing (data constructor used is usually `Nothing` or `None`) or some value (data constructor used is `Just` or `Some`).

```haskell
data Maybe a = Just a | Nothing
```

## Binary Tree
A Binary Tree is a Sum Type that is either Empty or a composition of a value and the tree's left and right subtrees. The value is the data of the current node. Here we are using Data Constructors `Empty` and `Node`.

```haskell
data Tree a = Empty | Node a (Tree a) (Tree a)
```
