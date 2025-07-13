{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Lib
  ( someFunc,
  )
where

import AST (Term (Unit))
-- import Control.Monad (forM_, when)

-- import Eval (eval, isFullyEvaluated, stageEval)

-- import System.Exit (exitSuccess)

import Data.IORef (newIORef, readIORef, writeIORef)
import Eval (evalC, evalR, show')
import Parse (parseFromCode)
import System.Exit (exitFailure)
import Text.RawString.QQ
import Typing (StageFlag (Runtime), infer)

code :: String
code =
  [r|
#let times =
  fix p: #(#Nat -> Nat).
  fun n: #Nat.
    #case n of
        0 => 0
      | s m => s(s(p#(m)))
in times#(8)
  |]

someFunc :: IO ()
someFunc = do
  termRef <- newIORef Unit
  putStrLn ">> Parsing term..."
  case parseFromCode code of
    Left err -> putStrLn err
    Right term -> print term >> writeIORef termRef term

  term <- readIORef termRef
  putStrLn ">> Elaborating term..."
  case infer Runtime ([], []) term of
    Left err -> do
      putStrLn $ red $ "Type error: " ++ show err
      exitFailure
    Right t -> putStrLn (green $ show t)

  putStrLn ">> Evaluating term..."
  let value = evalC ([], []) term
  putStrLn $ "Compile-time: \t" ++ green (show' value)
  let value = evalR ([], []) term
  putStrLn $ "Runtime: \t" ++ green (show value)

-- Colorful Output

green :: String -> String
green str = "\ESC[32m" ++ str ++ "\ESC[0m"

red :: String -> String
red str = "\ESC[31m" ++ str ++ "\ESC[0m"