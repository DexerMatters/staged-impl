{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Lib
  ( someFunc,
  )
where

import AST (Term (Unit))
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Sequence as Seq
import Eval (eval, quote, unbox)
import Parse (parseFromCode)
import Text.RawString.QQ
import Typing (infer)

code :: String
code =
  [r|
let add =
  fix f: (Nat, Nat) -> Nat.
  fun n: (Nat, Nat).
    case fst(n) of
        0    => snd(n)
      | s ns => f( (ns, s(snd(n))) )
in
let times = 
  fix p: Nat -> [[Nat -> Nat]].
  fun n: Nat.
    case n of
      0   => [[fun x:Nat. 0]]
    | s m => [[fun x:Nat. add(
                (x, (eval 1 (p(m)))(x))
              )]]
in 
[[(eval 1 (times(2)))(3)]]
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
  case infer Seq.Empty term of
    Left err -> putStrLn $ red $ "Type error: " ++ show err
    Right t -> putStrLn (green $ show t)

  putStrLn ">> Evaluating term..."
  let quoted = quote (eval [] term)
  putStrLn $ "Stage 0 (Compile Time): \n" ++ green (show quoted)
  putStrLn $ "Stage 1 (Run Time): \n" ++ green (show (eval [] (unbox quoted)))

-- Colorful Output

green :: String -> String
green str = "\ESC[32m" ++ str ++ "\ESC[0m"

red :: String -> String
red str = "\ESC[31m" ++ str ++ "\ESC[0m"