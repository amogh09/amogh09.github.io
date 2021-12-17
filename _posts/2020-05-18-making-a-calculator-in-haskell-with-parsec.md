---
layout: post
title: Making a Calculator in Haskell with Parsec
date: 2020-05-18
tags: haskell functional-programming
---
In this article, I will show you how to use [Parsec](https://wiki.haskell.org/Parsec) in an Applicative style to parse and evaluate simple expressions. We will make a Calculator in Haskell with Parsec library. Our calculator will support addition, subtraction, multiplication, division, and parentheses with their respective precedences. This article is aimed at showing the power of Parsec and Parsec-like libraries and the elegance of Applicative programming. Read our introductory piece on [Applicatives]({% post_url 2020-05-12-applicative-functor-for-beginners %}) for more information on Applicative Functors.

## Imports
We'll use the following imports for our program.
```haskell
-- calculator.hs
import Text.Parsec (char, between, digit, many1)
import Text.Parsec.String (Parser)
import Data.Char (digitToInt)
import Control.Applicative (optional, (<|>), many)
```

## Parsing decimals
Parsec does not come with decimal parser out of the box so first let's define a decimal parser. We simply parse one or more digits and then convert the parsed string to the integer it represents.

```haskell
-- calculator.hs
decimal :: Parsec String () Int 
decimal = atoi <$> many1 digit

atoi :: [Char] -> Int 
atoi = foldl f 0 where 
    f s x = 10*s + digitToInt x
```

We use the `many1` function from `Text.Parsec` that parses one or more occurrences of whatever pattern you give it. Here we are using it to parse one or more digits. Next, we map the parsed digits in string format to an integer using `atoi` function. `atoi` function folds the digits to produce the integer they represent. For each new digit we shift the current integer to left (multiply by 10) and add the new digit to it.

## Calculator Grammar
We'll use the popular calculator grammar to define our parsing rules later. The snippet below shows the grammar in <a href="https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form">Backus-Naur form (BNF)</a>.

```
Expr   ::= Term ('+' Term | '-' Term)*
Term   ::= Factor ('*' Factor | '/' Factor)*
Factor ::= ['-'] (Number | '(' Expr ')')
Number ::= Digit+
```

According to our grammar above, an expression `Expr` is either a single term `Term` or multiple terms joined with `+` or `-` operators. Addition and Subtraction are handled at `Expr` level. A `Term` is either a single `Factor` or multiple `Factor`s joined with `*` or `/` operators. Multiplication and division are handled at `Term` level. A `Factor` is either a number or an expression inside a parentheses. `Factor` can optionally be prefixed with `-` sign to make it negative. Parentheses are handled at `Factor` level. `Number` is defined as one or more digits representing an integer.

## Implementing the grammar in Parsec
Now it's time to write some Haskell to implement the above grammar using Parsec.

### Expr
```haskell-- calculator.hs
expr :: Parser Int
expr = pure eval <*> term <*> many (pure (,) <*> (char '+' <|> char '-') <*> term)
```

First is the `expr :: Parser Int` Parser that corresponds to `Expr` symbol in our grammar. Type `Parser` is imported from `Text.Parsec.String` module and represents a parser of `String` type. So `Parser Int` is a parser of `String` that produces an `Int`.

Now consider `term <*> many (pure (,) <*> (char '+' <|> char '-') <*> term)` part of our parser definition. Here we parse a `term` (we'll define it later) that's followed by zero or more (`many`) `(char '+' <|> char '-') <*> term` parts. This is exactly how we defined `Expr` symbol in our grammar. We use the choice function `<|>` function from `Control.Applicative` to tell Parsec to parse either a `+` or a `-` character (but not both). We use the `<*>` function from `Control.Applicative` to lift a pure function `(,)` to apply it to the character produced by `(char '+' <|> char '-')` and the `term` produced the following `term` parser to wrap them in a tuple. The type of `term` as we'll see later is `Parser Int` so `pure (,) <*> (char '+' <|> char '-') <*> term` gives us a parser of type `Parser (Char, Int)`. `many` function from Applicative module is applied here to tell Parsec to keep parsing `(Char, Int)` as long as it can. Thus, applying `many` to `Parser (Char, Int)` type gives us a parser of type `Parser [(Char, Int)]`.

Next, we need to combine the `Parser Int` from our first `term` and `Parser [(Char, Int)]` into a single `Parser Int`. We do that by applying a pure function `eval :: Int -> [(Char,Int)] -> Int` (we'll define this later) that will fold the parsed integers and operators into a single integer by adding/subtracting the integers as appropriate. Once again we use `<*>` function to apply `eval` on values wrapped in Applicatives (`Parser`s).

### Term
```haskell
-- calculator.hs
term :: Parser Int 
term = pure eval <*> factor <*> many (pure (,) <*> (char '*' <|> char '/') <*> factor)
```

`term` parser is very similar to `expr` just like the `Term` symbol is similar to `Expr` symbol in our grammar. Here instead of parsing `+` and `-` operators we parse `*` and `/` instead. Instead of parsing `term`s we parse `factor`s as described in our grammar.

```haskell
-- calculator.hs
eval :: Int -> [(Char,Int)] -> Int 
eval x [] = x 
eval x (('+', x'):xs) = eval (x + x') xs
eval x (('-', x'):xs) = eval (x - x') xs
eval x (('*', x'):xs) = eval (x * x') xs
eval x (('/', x'):xs) = eval (x `div` x') xs
```

Now we define `eval` function as shown above. It is a simple folding function that has an aggregator to which it adds/subtracts/multiplies/divides integers from the list depending on what operator they carry with them.

### Factor
```haskell
-- calculator.hs
factor :: Parser Int 
factor = pure f <*> optional (char '-') <*> (decimal <|> between (char '(') (char ')') expr)
    where 
        f Nothing x = x 
        f _ x = negate x
```

Our final piece is the `factor` parser. With `(decimal <|> between (char '(') (char ')') expr)` it parses either a `decimal` or an `expr` wrapped in parentheses. It also parses an optional `-` character before the decimal or expression using the `optional` function from `Control.Applicative` module. This is for negation of the decimal or the expression, of course. Finally, we produce a value by applying `f` function on the optional negation operator and the parsed integer value. `f` simply negates the parsed integer value if the `-` operator was parsed by the parser.

## Showtime
And that's it! Let's give our tiny calculator a go.

```haskell
-- ghci
λ> :l calculator.hs 
[1 of 1] Compiling Main             ( calculator.hs, interpreted )
Ok, one module loaded.
λ> import Text.Parsec (parse)
λ> parse expr "" "1+1"
Right 2
λ> parse expr "" "1+1-2"
Right 0
λ> parse expr "" "1+2*3*2-2"
Right 11
λ> parse expr "" "1+2*3*(2-2)"
Right 1
```

Looks like it works! You can try adding support for more operators such as exponentiation, log, square root etc.

## Summary
Today we saw some Applicative coding in action for making a Calculator app in Haskell with Parsec. You might have noticed that we used no `let` declarations or `do` notations anywhere in our code. This is a typical benefit of Applicative style of coding. `<*>` is a powerful function that allows us to write compact and succinct code that has a great functional feel. Happy Haskelling!

## Appendix - Source code
The complete calculator program is reproduced below.

```haskell
-- calculator.hs
import Text.Parsec (char, between, digit, many1)
import Text.Parsec.String (Parser)
import Data.Char (digitToInt)
import Control.Applicative (optional, (<|>), many)

atoi :: [Char] -> Int 
atoi = foldl f 0 where 
    f s x = 10*s + digitToInt x

decimal :: Parser Int 
decimal = atoi <$> many1 digit

expr :: Parser Int
expr = pure eval <*> term <*> many (pure (,) <*> (char '+' <|> char '-') <*> term)

term :: Parser Int 
term = pure eval <*> factor <*> many (pure (,) <*> (char '*' <|> char '/') <*> factor)

eval :: Int -> [(Char,Int)] -> Int 
eval x [] = x 
eval x (('+', x'):xs) = eval (x + x') xs
eval x (('-', x'):xs) = eval (x - x') xs
eval x (('*', x'):xs) = eval (x * x') xs
eval x (('/', x'):xs) = eval (x `div` x') xs

factor :: Parser Int 
factor = pure f <*> optional (char '-') <*> (decimal <|> between (char '(') (char ')') expr)
    where 
        f Nothing x = x 
        f _ x = negate x
```
