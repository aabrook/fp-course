{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Monad
import Course.Functor
import Course.List
import Course.Optional

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: Chars -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

To test this module, load ghci in the root of the project directory, and do
    >> :main "share/files.txt"

Example output:

$ ghci
GHCi, version ...
Loading package...
Loading ...
[ 1 of 28] Compiling (etc...
...
Ok, modules loaded: Course, etc...
>> :main "share/files.txt"
============ share/a.txt
the contents of a

============ share/b.txt
the contents of b

============ share/c.txt
the contents of c

-}

-- /Tip:/ use @getArgs@ and @run@
main ::
  IO ()
main =
  do
    run "share/files.txt"
type FilePath =
  Chars

-- /Tip:/ Use @getFiles@ and @printFiles@.
run ::
  FilePath
  -> IO ()
run =
  (printFiles =<<) . (getFiles . lines . snd =<<) . getFile
-- printFiles =<< (getFiles . lines . snd) =<< getFile start
{-
  getFile start >>=
    (getFiles . lines . snd) >>=
    printFiles
-}
{-
  do
    (_name, f) <- getFile start
    files <- getFiles (lines f)
    printFiles files
-}

getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
getFiles =
  sequence . (getFile <$>)

getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile =
  lift2 (<$>) (,) readFile
-- (\c -> (name, c)) <$> readFile name
{-
  do
    f <- readFile name
    pure (name, f)
-}
printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles =
  traverse_ (uncurry printFile)
-- traverse_ (\(a, b) -> printFile a b)
-- void $ sequence ((\(a, b) -> printFile a b) <$> files)

printFile ::
  FilePath
  -> Chars
  -> IO ()
printFile name contents =
  traverse_ putStrLn (name :. contents :. Nil)

traverse_ :: Applicative f => (a -> f b) -> List a -> f ()
traverse_ f = void . sequence . (<$>) f
