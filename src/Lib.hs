{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Lib
  ( someFunc,
  )
where

import AST (Term (Unit))
import Control.Monad (forM_, when)
import Data.IORef (newIORef, readIORef, writeIORef)
import Eval (eval, isFullyEvaluated, stageEval)
import Parse (parseFromCode)
import System.Exit (exitSuccess)
import Text.RawString.QQ
import Typing (infer)

code :: String
code =
  [r|

let eval =
  fun x: [[Nat]].
    let box u = x
    in u
in
let liftTuple =
  fun x: ([[Nat]], [[Nat]]).
    let box u = fst(x)
    in let box v = snd(x)
       in [[(u, v)]]
in
liftTuple(([[1]], [[2]]))
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
  case infer ([], []) term of
    Left err -> putStrLn $ red $ "Type error: " ++ show err
    Right t -> putStrLn (green $ show t)

  putStrLn ">> Evaluating term..."
  let value = eval ([], []) term
  let stages = iterate stageEval value
  forM_ (zip [1 :: Int ..] stages) $ \(i, v) -> do
    putStrLn $ "Stage " ++ show i ++ ": " ++ green (show v)
    when (isFullyEvaluated v) exitSuccess

-- Colorful Output

green :: String -> String
green str = "\ESC[32m" ++ str ++ "\ESC[0m"

red :: String -> String
red str = "\ESC[31m" ++ str ++ "\ESC[0m"