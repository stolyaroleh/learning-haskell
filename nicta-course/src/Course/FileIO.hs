{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TupleSections #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Monad
import Course.Functor
import Course.List

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

$ runhaskell FileIO.hs "files.txt"
============ a.txt
the contents of a

============ b.txt
the contents of b

============ c.txt
the contents of c

-}

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
(g >=> f) a = join $ f <$> g a

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f = ((return . f) =<< )

-- /Tip:/ use @getArgs@ and @run@
main ::
  IO ()
-- main = do
--   args <- getArgs
--   run (head args)
--   where head (file :. _) = file
--         head Nil = error "No arguments provided"

main =
  liftM head getArgs >>=
  run
  where head (file :. _) = file
        head Nil = error "No arguments provided"

type FilePath =
  Chars

-- /Tip:/ Use @getFiles@ and @printFiles@.
run ::
  Chars
  -> IO ()
run file = getFile file >>=
  (\(_, text) -> getFiles (lines text)) >=>
  printFiles

mapM :: Monad m => (a -> m b) -> List a -> m (List b)
mapM f = sequence . map f

getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
getFiles =
  mapM getFile

getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile name = (name,) <$> readFile name

printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles = let printFile' = uncurry printFile
             in void . mapM printFile'

printFile ::
  FilePath
  -> Chars
  -> IO ()

printFile name contents = do
  putStrLn (replicate 12 '=' ++ name)
  putStrLn contents

-- printFile name contents =
--   putStrLn (replicate 12 '=' ++ name) >>=
--   \_ -> putStrLn contents
