{-# LANGUAGE CPP #-}

import AST (Term (Unit))
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Sequence as Seq
import Parse (parseFromCode)
import Typing (infer)

main :: IO ()
main = putStrLn "nothing to see here"