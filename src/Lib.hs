{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Lib
  ( someFunc,
  )
where

import AST (Term (Unit))
-- import Control.Monad (forM_, when)
import Data.IORef (newIORef, readIORef, writeIORef)
-- import Eval (eval, isFullyEvaluated, stageEval)
import Parse (parseFromCode)
-- import System.Exit (exitSuccess)
import Text.RawString.QQ
import Typing (InferFlag (CompTime, Runtime), infer)

code :: String
code =
  [r|
#let f = fun x : #Nat. x
in f#(1)
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
    Left err -> putStrLn $ red $ "Type error: " ++ show err
    Right t -> putStrLn (green $ show t)

-- putStrLn ">> Evaluating term..."
-- let value = eval ([], []) term
-- let stages = iterate stageEval value
-- forM_ (zip [1 :: Int ..] stages) $ \(i, v) -> do
--   putStrLn $ "Stage " ++ show i ++ ": " ++ green (show v)
--   when (isFullyEvaluated v) exitSuccess

-- Colorful Output

green :: String -> String
green str = "\ESC[32m" ++ str ++ "\ESC[0m"

red :: String -> String
red str = "\ESC[31m" ++ str ++ "\ESC[0m"