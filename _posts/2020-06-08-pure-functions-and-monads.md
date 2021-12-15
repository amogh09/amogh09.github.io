---
layout: post
title:  "Pure functions and Monads"
date:   2020-06-08 18:30:17 -0500
categories: haskell functional-programming
---
In today's article I'll be showing you a way to write pure functions that are Monad-friendly meaning that they are composable with Monadic contexts.

## Imports and Extensions
Let's first get the imports and language extensions we'd be using for this article out of the way.

```haskell
-- app.hs
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import           Control.Monad          (forever)
import           Control.Monad.Except   (ExceptT, MonadError, catchError,
                                         runExceptT, throwError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.State    (MonadState, StateT, evalStateT, get,
                                         modify)
import           Numeric                (readDec)
import           System.IO              (BufferMode (NoBuffering),
                                         hSetBuffering, stdout)
```

Cool. We can get started now!

## Pure functions and Monads
Say you want to write a pure function that has a possibility of failing. What type signature would you give it? One of the common ones is `Either String a`. For example, let's write a function that given a list of integers returns their average.

```haskell
-- app.hs
avg' :: [Int] -> Either String Double
avg' [] = Left "Cannot take average of empty list"
avg' xs = Right $ fromIntegral (sum xs) / fromIntegral (length xs)
```

Functions with concrete return types such as `Either String a` are not directly compatible with Monadic contexts other than `Either String`. Suppose we have an `App` monad for our application defined as follows.

```haskell
-- app.hs
newtype App s m e a = App
  { unapp :: StateT s (ExceptT e m) a
  } deriving (Functor, Applicative, Monad, MonadError e, MonadIO, MonadState s)
```

Calling our `avg'` function from within `App` context is not straightforward as the `Either e` monad is not compatible with our `App` monad out of the box. We'll need to write a function that can lift a value of type `Either e a` to `App s m e a` type. Although writing such a function is possible, we can do better.

Instead of coding pure functions to concrete types, we can code them to typeclasses. The typeclass that abstracts the functionality offered by `Either e` type is `MonadError e`. Let's redefine our average function so that it operates within `MonadError` context.

```haskell
-- app.hs
avg :: MonadError String m => [Int] -> m Double
avg [] = throwError "Cannot take average of empty list"
avg xs = pure $ fromIntegral (sum xs) / fromIntegral (length xs)
```

What did we change? Notice that we no longer return `Either String Double` anymore. Instead, we return a `Double` within a `MonadError String` context. This `MonadError String` context could be any `MonadError String` instance! Notice that our `App s m e` monad is an instance of `MonadError e` making it compatible with our new `avg` function when `e = String`

We have also replaced `Left` with `throwError` and `Right` with `pure`. For `Either e a` monad the functionality is exactly the same as before but now our function is more generic.

Let's confirm that `avg` function does indeed work within different `MonadError String` contexts.

```"haskell
-- ghci
λ> avg [1,2,3] :: Either String Double
Right 2.0
λ> let x = avg [1,2,3] :: Monad m => App s m String Double
λ> :t x
x :: Monad m => App s m String Double
```

Nice.

## Applying the knowledge
Let's now write a simple application that would demonstrate the usefulness of Monad compatible pure functions. We will write a console app to print online averages of integers. The app will continuously keep asking the user for integer values and print the average of all values collected until now after each integer is read.

```
Enter an integer: 5
Current avg: 5.0
Enter an integer: 0
Current avg: 2.5
Enter an integer: abc
Could not parse "abc" to an int
Enter an integer: -2
Current avg: 1.0
```

We will use `State [Int]` monad to store the current state of integers collected. So, our app will run in `App [Int] IO String` monadic context. Let's declare a type alias for this type for convenience.

```"haskell
type MyApp = App [Int] IO String
```

To run our `App s m e` monad, we will need to unwrap and run all its transformers one by one.

```haskell
-- app.hs
runapp :: Monad m => s -> App s m e a -> m (Either e a)
runapp s = runExceptT . flip evalStateT s . unapp
```

Now let's define a `readInt` function that will try to parse an integer from a string while handling any errors. For this we will use `readDec` function from `Numeric` module with some modifications.

```haskell
-- app.hs
readInt :: MonadError String m => String -> m Int
readInt []       = throwError "Cannot read int from empty string"
readInt ('-':xs) = negate <$> readInt xs -- Handle negative integers
readInt xs       = case readDec xs of
  [(v, "")] -> pure v
  _         -> throwError $ "Could not parse " <> show xs <> " to an int"
```

Note that we have defined `readInt` function within `MonadError String` context just like the `avg` function.

Next, we define a function `getInt` that will ask the user to input an integer, try to parse the input, and ask the user to input again if there was any error.

```haskell
-- app.hs
getInt' :: MyApp Int
getInt' =
      liftIO (putStr "Enter an integer: ")
  *>  liftIO getLine
  >>= readInt

getInt :: MyApp Int
getInt = getInt' `catchError` \e -> (liftIO . putStrLn $ e) *> getInt
```

Note how we are able to call our pure `readInt` function from within `MyApp` monad without any lifting.

Alright, we have all the pieces we need. Now let's define a loop that would ask the user for input and display the current averages.

```haskell
-- app.hs
go :: MyApp ()
go = forever $ do
  x <- getInt       -- get int from user
  modify (x:)       -- prepend new int to our state
  y <- avg =<< get  -- compute the average of collected ints
  liftIO $ putStrLn $ "Current avg: " <> show y
```

Once again, we are able to call pure `avg` function from `MyApp` context without having to perform any lift juggling.

And that's it. We can now call `go` from our `main` function to start the app.

```haskell
-- app.hs 
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- so that everything is printed right away
  runapp [] go >>= either print (const $ pure ())
```

## Conclusion and follow-ups
We saw today how pure functions can be made more generic so that they may be called from monadic contexts without much trouble.

You might have noticed that `MonadError String` is not completely generic as it assumes the error type to be `String`. We cannot get rid of the concrete error type as we need it to create the error value.

However, we can make our app monad an instance of `Bifunctor` to easily convert a value of type `App s m e a` to `App s m e' a`. The function that does this is called <a href="https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Bifunctor.html#v:first">first</a>. I will cover <a href="https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Bifunctor.html">Bifunctor</a> in detail in a separate blog post.

## Appendix
Complete program is reproduced below if you want to copy it. ;)

```haskell
-- app.hs
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import           Control.Monad          (forever)
import           Control.Monad.Except   (ExceptT, MonadError, catchError,
                                         runExceptT, throwError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.State    (MonadState, StateT, evalStateT, get,
                                         modify)
import           Numeric                (readDec)
import           System.IO              (BufferMode (NoBuffering),
                                         hSetBuffering, stdout)

avg :: (MonadError String m) => [Int] -> m Double
avg [] = throwError "Cannot take average of empty list"
avg xs = pure $ fromIntegral (sum xs) / fromIntegral (length xs)

readInt :: MonadError String m => String -> m Int
readInt []       = throwError "Cannot read int from empty string"
readInt ('-':xs) = negate <$> readInt xs -- Handle negative integers
readInt xs       = case readDec xs of
  [(v, "")] -> pure v
  _         -> throwError $ "Could not parse " <> show xs <> " to an int"

newtype App s m e a = App
  { unapp :: StateT s (ExceptT e m) a
  } deriving (Functor, Applicative, Monad, MonadError e, MonadIO, MonadState s)

type MyApp = App [Int] IO String

runapp :: Monad m => s -> App s m e a -> m (Either e a)
runapp s = runExceptT . flip evalStateT s . unapp

getInt' :: MyApp Int
getInt' =
      liftIO (putStr "Enter an integer: ")
  *>  liftIO getLine
  >>= readInt

getInt :: MyApp Int
getInt = getInt' `catchError` \e -> printError e *> getInt
  where printError e = liftIO $ putStrLn e

go :: MyApp ()
go = forever $ do
  x <- getInt
  modify (x:)
  y <- avg =<< get
  liftIO $ putStrLn $ "Current avg: " <> show y

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  runapp [] go >>= either print (const $ pure ())
```
