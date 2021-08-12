*This article presumes beginner level knowledge of Haskell programming language on part of the reader. To get the most out of this article some knowledge of basic Haskell syntax, basic data types like Maybe and Either, and Functor typeclass will be highly beneficial.*

![unfold](https://fpunfold.com/wp-content/uploads/2020/05/haskell_unfold.001-300x176.jpeg)

## What this post covers?
After reading this post you'll have a basic understanding of Applicative Functors. You'll also see some relatable real-life problems where Applicative Functors can come in handy and yield an elegant solution.

## Applicative Functor
In an earlier article I explained about [Functors](https://fpunfold.com/2020/05/08/functor/). A quick refresher is that a Functor is a container or a context whose value(s) could be mapped over with a function to produce a new Functor. This is done using the `fmap` function. Haskell defines the following typeclass for Functors.

```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```

Applicative Functor is a Functor that has a few more tricks up its sleeve. List, Maybe, Either all are Applicative Functors as we'll discover later. First let's see the extra functions that it supports.

```haskell
class (Functor f) => Applicative f where 
    pure  :: a -> f a 
    (<*>) :: f (a -> b) -> f a -> f b
```

`(Functor f) =>` just means that any Applicative must also be a Functor; it is a constraint. The first function we have is `pure :: a -> f a`. This is a simple function that takes any value and returns an Applicative Functor that wraps the value. You can think of it as providing a <emp>pure</emp> (simple or trivial) context for the value. For lists, this function returns a list with a single element (a singleton list). For `Maybe`, it wraps the value in a `Just`. And for an `Either` it wraps the value in a `Right`. I hope there are no surprises here.

```
-- ghci (Haskell shell)
λ> pure "fpunfold" :: Maybe String
Just "fpunfold"
λ> pure "fpunfold" :: [String]
["fpunfold"]
λ> pure "fpunfold" :: Either String String
Right "fpunfold"
```

The second function Applicative Functor supports is `(<*>) :: f (a -> b) -> f a -> f b`. As the type signature suggests, `<*>` takes an Applicative (short for Applicative Functor) that contains a function of type `a -> b` and applies it to a value inside another Applicative `f a`, while merging the contexts of the two Applicatives. Hmm.. who in their right mind would wrap a function inside a container like Applicative?. This might look funky at first but trust me it'll make much more sense later when you see it in action with some examples. But before that let's see how `Maybe` is an Applicative.

## Maybe type as an Applicative

`Maybe` has the following implementation for Applicative typeclass (actual implementation in Haskell source is slightly different but does the exact same thing).

```haskell
instance Applicative Maybe where
    pure x = Just x

    Just f <*> Just x = Just (f x)
    _ <*> _           = Nothing
```

`pure` function just wraps the value in `Just`. For `<*>` it makes sense that we can perform function application only when both the function and the value are available inside the two `Maybe`s provided. If either isn't available then the result is `Nothing`. Let's confirm that this does what we expect.

```
-- ghci
λ> Just (\x -> x + 1) <*> Just 3
Just 4
λ> Just (\x -> x + 1) <*> Nothing
Nothing
λ> Nothing <*> Just 3
Nothing
λ> Nothing <*> Nothing
Nothing
```

Phew! after all this theory I can finally show you some example of when this is useful. Recall that functions in Haskell are [https://fpunfold.com/2020/05/09/why-haskell-for-functional-programming/#currying](Curried) by default meaning that passing one argument to a multi-argument function returns another function that takes one less parameter. You can think of this as partially applying functions. So, when you do `(+) 2` it creates a new function that takes one integer and adds 2 to it. What do you think would happen if we do `Just (+) <*> Just 2`? Let's analyze its type signature in ghci using `:t` command.

```
// ghci
λ> :t Just (+) <*> Just 2
Just (+) <*> Just 2 :: Num a => Maybe (a -> a)
```

We see that the signature of the function returned by `<*>` is `Num a => Maybe (a -> a)`, which is a function wrapped inside a new `Maybe` Applicative Functor. Now we can pass it to `<*>` again with a fresh argument value!

```
λ> Just (+) <*> Just 2 <*> Just 4
Just 6
```

We now have a result value wrapped in `Maybe` since our function `(+)` has been passed all the parameters it requires (2 and 4 in the example above). What does this mean? It means that we can take any arbitrary function, wrap it in a `Maybe` (called "lifting to Applicative") and apply it successively to arguments wrapped in `Maybe`. If at any stage we see an argument that is `Nothing`, the result will automatically be `Nothing`. This simplifies and standardizes error handling!

```haskell
λ> Just (+) <*> Just 2 <*> Just 4
Just 6
λ> Just (+) <*> Nothing <*> Just 4
Nothing
λ> Just (+) <*> Just 2 <*> Nothing
Nothing
```

Now let's see a more relatable example. Suppose we want to parse parameters passed in a query string (a string containing key-value pairs). Say we want to parse the query string `firstName=fp&lastName=unfold&age=0&email=contact@fpunfold.com` to a data type representing a Person. Our function should account for missing information in the encoded string and should fail gracefully in such cases. Let's define our `Person` data type and write the type signature for our function.

```haskell
-- applicative.hs
data Person = Pers {
        _firstName :: String 
    ,   _lastName  :: String 
    ,   _age       :: Int 
    ,   _email     :: String
    } deriving Show

parseMaybe :: String -> Maybe Person
```

Our data type `Person` contains four required fields that we want to parse from a query string. Function `parseMaybe` is our parsing function that tries to parse the passed query string and returns a `Just Person` if it was successful, and returns `Nothing` on any failure like insufficient information in the query string. To do that, we'll first extract all (key, value) tuples from the query string using a `decode` function as shown below. We are using `splitOn` function from module Data.List.Split here. You can download the package using `cabal install split` command if you don't have it available.

```haskell
-- applicative.hs
import Data.List.Split (splitOn)

decode :: String -> [(String,String)] 
decode encoded = 
    fmap toTup . filter validTup . fmap (splitOn "=") . splitOn "&" $ encoded
    where 
        toTup [x,y] = (x,y)
        validTup xs = length xs == 2
```

`decode` function first splits the encoded string on "&" character. Each part in the result is now expected to be a key-value pair separated by "=" character. So, we split each part on "=" character (mapping with `splitOn "="`) and filter out ones that do not contain exactly two elements after the split as those parts are invalid. Then, we just map all two-element lists (key-value pairs) to tuples. Let's test the function in ghci.

```
// ghci
λ> :l applicative.hs 
[1 of 1] Compiling Main             ( applicative.hs, interpreted )
Ok, one module loaded.
λ> decode "firstName=fp&lastName=unfold&age=0&email=contact@fpunfold.com"
[("firstName","fp"),("lastName","unfold"),("age","0"),("email","contact@fpunfold.com")]
λ> decode "firstName=fp&this=part=is=invalid"
[("firstName","fp")]
λ> decode ""
[]
```

Now we create a `parsem :: [(String,String)] -> Maybe Person` function that'll try to create a `Person` from the key-value pairs. It uses Applicative Functor functions of `Maybe` to elegantly handle any errors.

```haskell
-- applicative.hs
parsem :: [(String,String)] -> Maybe Person 
parsem kvs = pure Pers 
    <*> lookup "firstName" kvs
    <*> lookup "lastName" kvs
    <*> fmap read (lookup "age" kvs)
    <*> lookup "email" kvs
```

And that's all! Our `parsem` function looks for all the required fields in the passed key-value pairs using `lookup :: Eq k => k -> [(k, v)] -> Maybe v` function that returns the value corresponding to the passed key if found or `Nothing` otherwise. To handle any missing information automatically we make use of `<*>` function. First, we lift the `Pers` Data Constructor (which is just a function that creates a `Person`) using `pure` (which is `Just` for `Maybe` Applicative) and then we partially apply it on arguments wrapped in `Maybe` using `<*>`. If at any point an optional argument is `Nothing`, `<*>` returns `Nothing` and then the whole chain evaluates to `Nothing`. For age of the person we first lookup age in string format using `lookup` function and then convert it to an `Int` by mapping it with `read` function. Now, our final `parseMaybe :: String -> Maybe Person` function is just a composition of `parsem` and `decode`.

```haskell
-- applicative.hs
parseMaybe :: String -> Maybe Person 
parseMaybe = parsem . decode
```

```
// ghci
λ> parseMaybe "firstName=fp&lastName=unfold&age=0&email=contact@fpunfold.com"
Just (Pers {_firstName = "fp", _lastName = "unfold", _age = 0, _email = "contact@fpunfold.com"})
λ> parseMaybe "firstName=fp&lastName=unfold&age=0" -- missing email
Nothing
λ> parseMaybe "firstName=fp&&email=contact@fpunfold.com" -- missing lastName and age
Nothing
```

## Either as an Applicative

Similar to `Maybe`, `Either l` is also an Applicative Functor as shown below.

```haskell
instance Applicative (Either e) where
    pure          = Right

    Left  e <*> _ = Left e
    Right f <*> r = fmap f r
```

Implementation is similar to `Maybe`. `<*>` performs function application only if both sides are `Right`. Otherwise it returns `Left` value from first or second parameter in that order.

Our `parsem` function works well but it doesn't tell us much about failures. What if we want to know what field was missing? Is there another type that's like `Maybe` but slightly more informative? It's the `Either` type! We can use `Either String Person` instead of `Maybe Person` so that we can return some error in string format if something fails. And since `Either String` is an Applicative our code remains almost the same.

```haskell
-- applicative.hs
eitherMaybe :: l -> Maybe r -> Either l r
eitherMaybe e Nothing  = Left e 
eitherMaybe _ (Just x) = Right x

parsee :: [(String,String)] -> Either String Person 
parsee m = pure Pers
    <*> eitherMaybe "firstName absent" (lookup "firstName" m)
    <*> eitherMaybe "lastName absent" (lookup "lastName" m)
    <*> eitherMaybe "age absent" (fmap read . lookup "age" $ m)
    <*> eitherMaybe "email absent" (lookup "email" m)
```

Since `lookup` function returns a `Maybe` we first need an easy way to convert it to an `Either`. We define a simple `eitherMaybe` function to handle this. It takes an error value and a `Maybe`. For `Just x` it wraps the value in a `Right` and for `Nothing` it returns the passed error value wrapped in a `Left`. Next is our new parsing function `parsee`. It remains almost identical in structure to our `parsem` function. The only difference is that we convert the `Maybe` results of `lookup` function into `Either` and provide an informative error message if the lookup failed. The usage of Applicative's `<*>` function remains the same. Instead of working on `Maybe`s it works on `Either`s this time.

Let's also define a `parseEither :: String -> Either String Person` function like we defined `parseMaybe` before.

```haskell
-- applicative.hs
parseEither :: String -> Either String Person 
parseEither = parsee . decode
```

Let's give it a go in ghci.

```
// ghci
λ> :l applicative.hs 
[1 of 1] Compiling Main             ( applicative.hs, interpreted )
Ok, one module loaded.
λ> parseEither "firstName=fp&lastName=unfold&age=0&email=contact@fpunfold.com"
Right (Pers {_firstName = "fp", _lastName = "unfold", _age = 0, _email = "contact@fpunfold.com"})
λ> parseEither "firstName=fp&lastName=unfold&age=0" -- missing email
Left "email absent"
λ> parseEither "firstName=fp&&email=contact@fpunfold.com" -- missing lastName and age
Left "lastName absent"
```

Isn't this great?

## Summary
We saw what Applicative Functors are and how they can be useful in providing elegant solutions to some every day problems. In fact, Haskell language has some powerful and brilliant parsing tools based on something called Parser Combinators which in turn are based on Applicative functors! I will cover Parser Combinator in separate posts. We also haven't seen how other Applicatives such as Lists behave and what are some more interesting properties of Applicatives. All that will be future articles as well! I hope you learned a thing or two from this post and that your time here was well spent. :)
