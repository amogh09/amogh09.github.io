---
layout: post
title: Building real-world Haskell applications using final-tagless and ReaderT
date: 2023-01-30
tags: haskell real-world architecture final-tagless readert
---
I have been learning and playing with Haskell on-and-off for a couple of years now. However, I was still not very confident that I could write a real-world application in it. So, I decided to give it another shot. My criteria for a "real-world" application is -

1. interacts with the real-world (dowloads something from the Internet, for example)
1. implementation details are hidden from high-level functions and the low-level implementations are easily swappable (dependency injection), and
1. dependencies can be mocked-out for unit testing.

The patterns I describe in this post are inspired from [The ReaderT Design Pattern](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/) and [Three Layer Haskell Cake](https://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html) articles.

## How does the OO world do it?
Dependency injection is a well known pattern in Object Oriented programming. The idea is to "program to interfaces" and inject implementations of interfaces during initialization. This pattern is very well understood and we will be implementing this pattern in Haskell using final-tagless and ReaderT patterns.

## Final-tagless pattern
Final-tagless pattern suggests programming to typeclass constraints instead of concrete dependencies. It is basically the Haskell version of programming to interfaces. 

Say we want to write a function to download a wallpaper from the Internet and save it to local disk.

```haskell
import qualified Data.ByteString as BS
import qualified Network.HTTP.Simple as HTTP
import System.FilePath ((</>))

type URL = String

type WallpaperName = String

downloadWallpaper :: FilePath -> WallpaperName -> URL -> IO ()
downloadWallpaper dir name url = do
  wallpaper <- HTTP.httpBS (HTTP.parseRequest_ url) -- get the wallpaper
  let path = dir </> name
  BS.writeFile path (HTTP.getResponseBody wallpaper) -- save the wallpaper
```

This function is tightly coupled to `httpBS` and `writeFile`. If, instead of saving the wallpaper to the disk, we wanted to save it to a database, then we would need to write a new function entirely. 

With the final-tagless pattern we can make this function more flexible. The idea is to write typeclasses that export actions and program high-level functions to the methods of the typeclasses.

```haskell
{-# LANGUAGE FlexibleInstances #-}

import Data.ByteString (ByteString)

type URL = String

type WallpaperName = String

downloadWallpaper ::
  ( MonadGetWallpaper m, -- Provides a method to get the wallpaper
    MonadSaveWallpaper m -- Provides a method to save the wallpaper
  ) =>
  WallpaperName ->
  URL ->
  m ()
downloadWallpaper name url = do
  wallpaper <- getWallpaper url -- get the wallpaper
  saveWallpaper name wallpaper -- save the wallpaper

class Monad m => MonadGetWallpaper m where
  getWallpaper :: URL -> m ByteString

class Monad m => MonadSaveWallpaper m where
  saveWallpaper :: WallpaperName -> ByteString -> m ()
```

Now we can provide different instances for the typeclasses to get different effects from the same function. Note that the new `downloadWallpaper` function no longer has a directory parameter. It is now more high-level and is agnostic to low-level details of wallpaper saving.

## ReaderT pattern
Note that a single type is required to implement all typeclasses we introduce with the final-tagless pattern. With the ReaderT pattern, the type we choose to implement the typeclasses is `ReaderT Env IO`. `Env` will usually be a record type containing all configuration, state, and resources needed by our application.

For our running example, we'd need a directory to save the wallpapers. Let's create a record type `Env` that will contain a wallpaper directory as configuration and make `MonadIO m => ReaderT Env m` implement the two typeclasses.

```haskell
data Env = Env
  { wallpaperDir :: FilePath
  }

instance MonadIO m => MonadSaveWallpaper (ReaderT Env m) where
  saveWallpaper name wallpaper = do
    dir <- asks wallpaperDir
    liftIO $ saveWallpaperToDisk dir name wallpaper

instance MonadIO m => MonadGetWallpaper (ReaderT Env m) where
  getWallpaper = liftIO . getWallpaperFromURL

saveWallpaperToDisk :: FilePath -> WallpaperName -> ByteString -> IO ()
saveWallpaperToDisk dir name = BS.writeFile (dir </> name)

getWallpaperFromURL :: URL -> IO ByteString
getWallpaperFromURL =
  fmap HTTP.getResponseBody . HTTP.httpBS . HTTP.parseRequest_
```

Now we can run the `donwloadWallpaper` function as follows.

```haskell
runEnv :: IO ()
runEnv =
  runReaderT
    ( downloadWallpaper
        "wallhaven-kx36mq.png"
        "w.wallhaven.cc/full/kx/wallhaven-kx36mq.png"
    )
    (Env "wallpapers")
```

This allows us to write unit tests for the `downloadWallpaper` function using a test environment that, instead of making actual HTTP calls and saving data to the disk, would return fake wallpaper data from memory and save wallpapers to an `IORef` variable.

```haskell
data TestEnv = TestEnv
  { wallpaperData :: ByteString,
    savedWallpapers :: IORef [(WallpaperName, ByteString)]
  }

instance Monad m => MonadGetWallpaper (ReaderT TestEnv m) where
  getWallpaper _ = asks wallpaperData

instance (Monad m, MonadIO m) => MonadSaveWallpaper (ReaderT TestEnv m) where
  saveWallpaper name wallpaper = do
    wallpapers <- asks savedWallpapers
    liftIO $ modifyIORef wallpapers ((name, wallpaper) :)
```

A simple test would then look like below.

```haskell
import Test.Hspec (describe, hspec, it, shouldBe)

runTest :: IO ()
runTest = hspec $ do
  describe "downloadWallpaper" $ do
    it "saves the wallpaper to disk" $ do
      let name = "abc.jpg"
          wallpaperData = "wallpaper data"
          url = "wallpaper.com/abc.jpg"

      savedWallpapersIORef <- newIORef []
      let env = TestEnv wallpaperData savedWallpapersIORef

      runReaderT (downloadWallpaper name url) env

      savedWallpapers <- readIORef savedWallpapersIORef
      savedWallpapers `shouldBe` [(name, wallpaperData)]
```

## Has typeclasses
High-level application functions might also need access to configuration or state data. A Has typeclass is a pattern that allows extracting out some value from application's environment. Has typeclasses export a single method that returns the desired value. For example, if our application has a debug mode flag then we could write a `HasDebugMode` typeclass that exports a method to get the debug mode value. 

```haskell
class HasDebugMode env where
  debugMode :: env -> Bool
```

Note that `Has` typeclasses are to be implemented by the environment type `Env` and not by `ReaderT Env m` monad.

```haskell
data Env = Env
  { wallpaperDir :: FilePath,
    envDebugMode :: Bool
  }

instance HasDebugMode Env where
  debugMode = envDebugMode
```

Functions that need access to the debug mode flag should add the `HasDebugMode env` constraint. For example, if we want to add some debug logs to our `downloadWallpaper` function then we can add `MonadReader env m` and `HasDebugMode env` constraints to it and then have access to the debug mode using `asks debugMode` as shown below.

```haskell
downloadWallpaper ::
  ( MonadReader env m,
    HasDebugMode env,
    MonadGetWallpaper m, 
    MonadSaveWallpaper m,
    MonadIO m -- for priting debug logs
  ) =>
  WallpaperName ->
  URL ->
  m ()
downloadWallpaper name url = do
  debug <- asks debugMode
  when debug $ liftIO $ putStrLn "Downloading wallpaper..."
  wallpaper <- getWallpaper url
  saveWallpaper name wallpaper
```

So, the idea is to use final-tagless and ReaderT patterns together. These two patterns help us structure our applications in a modular fashion. High-level and low-level details of the application are well separated and loosely coupled through the typeclasses.

## Three layers + Environment
Another pattern that I found useful is to structure applications with three layers aka the three-layer cake. Layer-1 depends on Layer-2 which in-turn depends on Layer-3. 

### Layer 1
This layer is concerned about the high-level flow of the application. All functions in this layer are high-level and program to typeclasses. If the functions need access to specific configuration data then a `Has` typeclass should be added as a constraint. If the functions need a specific side-effect such as downloading some data then an appropriate typeclass constraint that exports the side-effect should be added to it. In our example from above, the `downloadWallpaper` function belongs to Layer-1.

#### Environment
Application environment is a part of Layer 1 and any functions in this layer that need access to environment values such as application configuration would need `MonadReader env m` and `Has MyConfiguration env` constraints.

As we saw before, the environment type, say `Env`, is a record that contains all configuration, resources, and state needed to run the application. It implements all the `Has` typeclasses and `ReaderT Env m` monad implements all typeclasses that expose methods with side-effects. For tests, we could write a separate test environment type, say `TestEnv`, that impelements mock functionality for the typeclasses that expose methods with side-effects. If we want to swap some implementation for our application, say save data to a database instead of disk, we could write a new environment type and implement the typeclasses as suitable for the new environment.

### Layer 2
The second layer is composed of the various typeclasses that Layer-1 functions program to. Each typeclass exposes a single piece of monadic functionality. The `MonadGetWallpaper` and `MonadSaveWallpaper` typeclasses from our example belong to Layer 2.

### Layer 3
The third layer is composed of library functions that are used by environments to implement the Layer-2 typeclasses. These functions are either pure or have a simple typeclass constraint such as `MonadIO`. `saveWallpaperToDisk` and `getWallpaperFromURL` functions from our example belong to Layer 3. These functions take everything they need as explicit parameters and there is no magic involved. Any long-lived resources such as file handles or database connections will be taken as parameters by Layer-3 functions.

## Exceptions
Now let's tackle the unpleasant yet necessary part of any real-world application - error handling. A real-world application must account for actions that can go wrong. A service the application is calling might be down or the disk might be full or there could be permission issues when performing an action.

First, I recommend throwing exceptions in real-world Haskell applications when an IO action fails. When I was new to Haskell I saw beautiful monads such as `Either` and `Expect` that seemed to make all error handling explicit but also elegant. I naively believed that I wouldn't have to deal with runtime exceptions ever and that Haskell is the best language. I do agree with the latter part, however, later I realized that Haskell does not have any magic for dealing with runtime exceptions and they are a fact of life in the Haskell world. See [this post from FP Complete](https://www.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell/) that explains why throwing exceptions is better than attempting to capture all possible exceptional cases in function types.

`saveWallpaperToDisk` Layer-3 function in our example can fail with an exception due to a lack of write permissions to the directory. In that case, there is nothing that this function can do and must let the exception propogate upwards. From Layer 1's perspective, the wallpaper failed to save, it is not concerened with the cause that is the lack of permissions, it is not even aware that the wallpaper is being saved to a file. To catch exceptions at Layer 1, let's create a sum type that captures everything that can go wrong. Typically, you will have one exception data constructor per monadic action in your Layer-2 typeclasses. 

```haskell
data AppException
  = WallpaperGetException String
  | WallpaperSaveException String
  deriving (Show, Typeable)

instance Exception AppException
```

Here we have defined a sum type `AppException` that will capture all known exceptions for our application. Our download wallpaper application can fail due to two reasons - wallpaper failing to be fetched from the URL and wallpaper failiing to save. Each type of exception will have an underlying cause that depends on the instances of our Layer-2 typeclasses being used for evaluation. Since Layer 1 is agnostic to the instances of the typeclasses, we are demanding that the instances encode the underlying errors in the `String` type.

Since Layer 1 and Layer 3 known exceptions are of different types, Layer 2 needs to perform a mapping from Layer 3's exception types to Layer 1's exception type.

```haskell
instance MonadIO m => MonadSaveWallpaper (ReaderT Env m) where
  saveWallpaper name wallpaper = do
    dir <- asks wallpaperDir
    saveWallpaperToDiskExcept dir name wallpaper -- use the new function

saveWallpaperToDiskExcept :: 
  MonadIO m => FilePath -> WallpaperName -> ByteString -> m ()
saveWallpaperToDiskExcept dir name wallpaper =
  liftIO $
    catch 
      (saveWallpaperToDisk dir name wallpaper) 
      (throwIO . handleIOError dir) -- map the IO exception to AppException

handleIOError :: FilePath -> IOError -> AppException
handleIOError dir e
  | isPermissionError e =
      WallpaperSaveException $ "no permission to save wallpaper to " <> show dir
  | otherwise = WallpaperSaveException $ show e
```

In the above example, we are mapping all `IOError` exceptions to `AppException`s. A user-friendly error message is generated for permissions issues and the default error messages are used for all other cases.

We can now handle `AppException`s in Layer 1.

```haskell
runEnvExcept :: IO ()
runEnvExcept =
  catch runEnv handleException

handleException :: AppException -> IO ()
handleException (WallpaperGetException msg) =
  putStrLn $ "Failed to get wallpaper: " <> msg
handleException (WallpaperSaveException msg) =
  putStrLn $ "Failed to save wallpaper: " <> msg

runEnv :: IO ()
runEnv =
  runReaderT
    ( downloadWallpaper
        "wallhaven-kx36mq.png"
        "http://w.wallhaven.cc/full/kx/wallhaven-kx36mq.png"
    )
    (Env "/" True) -- no permission to save to "/"
```

By running `runEnvExcept` in `cabal repl` we see that the permission error is handled gracefully by the application.

```
λ> runEnvExcept
Downloading wallpaper...
Failed to save wallpaper: no permission to save wallpaper to "/"
```

### Pure errors
Exceptions are unavoidable for impure code and it's better to embrace them than fight them. However, for pure code, I advise using `Either` or `Except` monads or a `MonadThrow` instance. 

When propogating a Layer-3 pure error from an impure Layer-2 function to a Layer-1 function, however, you are probably better off mapping the pure error to a Layer-1 exception and throwing it. This is because the impure Layer-2 function would already have exception throwing in its contract, so having a single way (throwing exceptions) of signaling failure is better than having two ways.

## Summary
That is all I have for this post. The application that inspired this post is [wallhaven-sync](https://github.com/amogh09/wallhaven-sync) that is a CLI for syncing wallpapers from [Wallhaven](https://wallhaven.cc/) website to my computer. Check it out! Final-tagless, ReaderT, Has typeclasses, and embracing exceptions have all helped me write a Haskell application that I am somewhat satisfied with. In the future I plan to explore Free monads to achieve similar or better results!
